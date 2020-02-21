import progressbar
from lxml import etree
import json
import pdb

# Two dimension exchange rate dictionary. Access exchange rates by currency and year like ratedf[currencyCode][year]
with open('ex_rates.json') as f:
    ratedf = json.load(f)

ratedf["GPB"] = ratedf["GBP"]
ratedf["gbp"] = ratedf["GBP"]
ratedf["EURO"] = ratedf["EUR"]
ratedf["Euro"] = ratedf["EUR"]
ratedf["Eur"] = ratedf["EUR"]
ratedf["CDN"] = ratedf["CAD"]
ratedf["usd"] = ratedf["USD"]
ratedf["GHC"] = ratedf["GHS"]
ratedf["ZMK"] = ratedf["ZMW"]
ratedf["USS"] = ratedf["USD"]
ratedf["USN"] = ratedf["USD"]
ratedf["BEF"] = ratedf["EUR"]
ratedf["FIM"] = ratedf["EUR"]
ratedf["KSH"] = ratedf["KES"]
ratedf["GIP"] = ratedf["GBP"]
ratedf["FKP"] = ratedf["GBP"]
ratedf["AON"] = ratedf["AOA"]
ratedf["UYI"] = ratedf["UYU"]
ratedf["NUL"] = {"2000": 0}


# Used for ambiguously structed arrays resulting from XML queries. If an array has any entries, take the first one.
def default_first(array):
    # If an array isn't empty, give us the first element
    return array[0] if array is not None and len(array) > 0 else None


# Used for ambiguous result default replacement. If value doesn't exist, replace it with the default.
def replace_default_if_none(value, default):
    if value is None:
        return default
    elif str.strip(value) == "":
        return default
    else:
        return value


# Used for ambiguous recoding. If code exists, try and use the dictionary to look up the result.
def recode_if_not_none(code, dictionary):
    if code is None:
        return None
    elif str.strip(code) == "":
        return None
    else:
        try:
            return dictionary[code]
        except KeyError:
            return None


# Used for currency conversion. Works like recode_if_not_none but for our 2-dimension exchange rate dictionary
def convert_usd(value, year, currency, ratedf):
    if value == 0:
        return 0
    elif value is None or year is None or currency is None:
        return None
    try:
        conversion_factor = ratedf[currency][str(year)]
        if conversion_factor > 0:
            return value*conversion_factor
        else:
            return None
    except KeyError:
        return None


# A class that will hold the flattening function and dictionary definitions
class IatiFlat(object):
    def __init__(self):
        self.header = ["year", "recipient_code", "transaction_type", "usd_disbursement", "original_value", "currency_code", "budget_or_transaction", "budget_type", "iati_identifier"]
        self.dictionaries = {}
        # Defaults, can be overwritten with next function
        self.dictionaries["ratedf"] = ratedf

    def define_dict(self, name, dictionary):
        self.dictionaries[name] = dictionary

    # Main flattening function here. Input is the XML root of the XML document, and output is an array of arrays with flattened data.
    def flatten_activities(self, root, publisher):
        for dictionary_name in ["ratedf"]:
            assert dictionary_name in self.dictionaries, "Missing dictionary: {}".format(dictionary_name)
        output = []
        try:
            version = root.attrib["version"]
        except KeyError:
            # Defaults to 2.02 if  the document happens to be missing an IATI version
            version = '2.02'

        # Find all activities
        activity_len = len(root.findall("iati-activity"))

        # Set up a quick progress bar for tracking processing; iterate through every activity
        bar = progressbar.ProgressBar()
        for i in bar(range(0, activity_len)):
            activity = root.xpath('iati-activity[%s]' % (i + 1) )[0]
            # Capture iati identifier
            iati_identifier = default_first(activity.xpath("iati-identifier/text()"))

            child_tags = [child.tag for child in activity.getchildren()]

            secondary_reporter = default_first(activity.xpath("reporting-org/@secondary-reporter"))
            secondary_reporter = replace_default_if_none(secondary_reporter, "0")

            defaults = {}
            default_tags = ["default-currency"]
            for tag in default_tags:
                if tag in list(activity.attrib.keys()):
                    defaults[tag] = activity.attrib[tag]
                elif tag in child_tags:
                    defaults[tag] = default_first(activity.xpath("{}/@code".format(tag)))
                else:
                    defaults[tag] = None
            troublesome_publishers = ["fco", "wwf-uk"]
            if publisher == "fco":
                defaults["default-currency"] = "GBP"

            # For every sector and every recipient in the activity, try and total the percentage splits
            activity_recipient_percentage = 0.0

            recipient_countries = activity.findall("recipient-country")
            activity_recipients = {}
            for recipient_country in recipient_countries:
                attribs = recipient_country.attrib
                attrib_keys = list(attribs.keys())
                percentage = attribs['percentage'] if 'percentage' in attrib_keys else None
                if percentage is not None:
                    percentage = percentage.replace("%", "")
                activity_recipient_percentage += float(percentage) if percentage not in ['', None] else 0.0
                code = attribs['code'] if 'code' in attrib_keys else None
                if code is not None:
                    activity_recipients[code] = float(percentage) if percentage not in ['', None] else None

            recipient_regions = activity.findall("recipient-region")
            for recipient_region in recipient_regions:
                attribs = recipient_region.attrib
                attrib_keys = list(attribs.keys())
                percentage = attribs['percentage'] if 'percentage' in attrib_keys else None
                if percentage is not None:
                    percentage = percentage.replace("%", "")
                activity_recipient_percentage += float(percentage) if percentage not in ['', None] else 0.0
                code = attribs['code'] if 'code' in attrib_keys else None
                if code is not None:
                    activity_recipients[code] = float(percentage) if percentage not in ['', None] else None

            # If percentages are greater than 100, rescale to 100. Also divide by 100 to make sure percentages run 0 to 1.00
            activity_recipient_percentage = max(activity_recipient_percentage, 100.0)

            for activity_recipient_code in activity_recipients:
                activity_recipients[activity_recipient_code] = (activity_recipients[activity_recipient_code] / activity_recipient_percentage) if (activity_recipients[activity_recipient_code]) is not None else None

            # If there's only one recipient, it's percent is implied to be 100
            if len(list(activity_recipients.keys())) == 1:
                activity_recipients[list(activity_recipients.keys())[0]] = 1

            if secondary_reporter == "0":
                has_transactions = "transaction" in child_tags
                if has_transactions:
                    transactions = activity.findall("transaction")

                    # Once through the transactions to find the recipient sum, recipient percents
                    transaction_recipients = {}
                    for transaction in transactions:
                        transaction_type_code = default_first(transaction.xpath("transaction-type/@code"))
                        if transaction_type_code in ["E", "D", "3", "4"]:

                            recipient_country_code = default_first(transaction.xpath("recipient-country/@code"))
                            recipient_region_code = default_first(transaction.xpath("recipient-region/@code"))

                            recipient_code = replace_default_if_none(recipient_country_code, recipient_region_code)

                            value = default_first(transaction.xpath("value/text()"))
                            try:
                                value = float(value.replace(" ", "")) if value is not None else None
                            except ValueError:
                                value = None

                            if value is not None:
                                if recipient_code is not None:
                                    transaction_recipients[recipient_code] = value

                    # If we turned up valid recipients, don't use the activity-level ones
                    use_activity_recipients = len(list(transaction_recipients.keys())) == 0

                    # Calc the sum of non-negative recips
                    transaction_recipient_value_sum = 0.0
                    for recipient_code in transaction_recipients:
                        transaction_recipient_value_sum += transaction_recipients[recipient_code] if (transaction_recipients[recipient_code] > 0.0) else 0.0
                    if transaction_recipient_value_sum > 0.0:
                        for recipient_code in transaction_recipients:
                            transaction_recipients[recipient_code] = transaction_recipients[recipient_code]/transaction_recipient_value_sum

                    # If there's only one recipient, it's percent is implied to be 100
                    if len(list(transaction_recipients.keys())) == 1:
                        transaction_recipients[list(transaction_recipients.keys())[0]] = 1

                    for transaction in transactions:
                        transaction_type_code = default_first(transaction.xpath("transaction-type/@code"))
                        if transaction_type_code in ["E", "D", "3", "4"]:
                            transaction_date = default_first(transaction.xpath("transaction-date/@iso-date"))
                            try:
                                year = int(transaction_date[:4]) if transaction_date is not None else None
                            except ValueError:
                                year = None

                            currency = default_first(transaction.xpath("value/@currency"))
                            currency = replace_default_if_none(currency, defaults["default-currency"])
                            if currency is not None:
                                currency = currency.replace(" ", "")
                            if publisher in troublesome_publishers:
                                currency = defaults["default-currency"]

                            value = default_first(transaction.xpath("value/text()"))
                            try:
                                value = float(value.replace(" ", "")) if value is not None else None
                            except ValueError:
                                value = None

                            recipient_country_code = default_first(transaction.xpath("recipient-country/@code"))
                            recipient_region_code = default_first(transaction.xpath("recipient-region/@code"))

                            recipient_code = replace_default_if_none(recipient_country_code, recipient_region_code)

                            budget_type = None
                            b_or_t = "Transaction"

                            if value and use_activity_recipients:
                                for activity_recipient_code in list(activity_recipients.keys()):
                                    activity_recipient_percentage = activity_recipients[activity_recipient_code]
                                    calculated_value = value*activity_recipient_percentage if (activity_recipient_percentage is not None) else None
                                    if currency in self.dictionaries["ratedf"]:
                                        converted_value = convert_usd(calculated_value, year, currency, self.dictionaries["ratedf"])
                                    else:
                                        pdb.set_trace()
                                    # ["year","recipient_code","transaction_type","usd_disbursement","budget_or_transaction","budget_type","iati_identifier"]
                                    row = [year, activity_recipient_code, transaction_type_code, converted_value, calculated_value, currency, b_or_t, budget_type, iati_identifier]
                                    output.append(row)
                            elif value and not use_activity_recipients:
                                if currency in self.dictionaries["ratedf"]:
                                    converted_value = convert_usd(value, year, currency, self.dictionaries["ratedf"])
                                else:
                                    pdb.set_trace()
                                # ["year","recipient_code","transaction_type","usd_disbursement","budget_or_transaction","budget_type","iati_identifier"]
                                row = [year, recipient_code, transaction_type_code, converted_value, value, currency, b_or_t, budget_type, iati_identifier]
                                output.append(row)

                    # Loop through budgets, and capture as close equivalents as we can to transactions
                    has_budget = "budget" in child_tags
                    if has_budget:
                        budgets = activity.findall("budget")
                        for budget in budgets:
                            transaction_type_code = None
                            if "type" in list(budget.attrib.keys()):
                                budget_type = budget.attrib["type"]
                            else:
                                budget_type = None

                            transaction_date = default_first(budget.xpath("period-start/@iso-date"))
                            try:
                                year = int(transaction_date[:4]) if transaction_date is not None else None
                            except ValueError:
                                year = None

                            value = default_first(budget.xpath("value/text()"))
                            try:
                                value = float(value.replace(" ", "")) if value is not None else None
                            except ValueError:
                                value = None
                            currency = default_first(budget.xpath("value/@currency"))
                            currency = replace_default_if_none(currency, defaults["default-currency"])
                            if currency is not None:
                                currency = currency.replace(" ", "")
                            if publisher in troublesome_publishers:
                                currency = defaults["default-currency"]

                            b_or_t = "Budget"

                            if value and use_activity_recipients:
                                for activity_recipient_code in list(activity_recipients.keys()):
                                    activity_recipient_percentage = activity_recipients[activity_recipient_code]
                                    calculated_value = value*activity_recipient_percentage if (activity_recipient_percentage is not None) else None
                                    if currency in self.dictionaries["ratedf"]:
                                        converted_value = convert_usd(calculated_value, year, currency, self.dictionaries["ratedf"])
                                    else:
                                        pdb.set_trace()
                                    # ["year","recipient_code","transaction_type","usd_disbursement","budget_or_transaction","budget_type","iati_identifier"]
                                    row = [year, activity_recipient_code, transaction_type_code, converted_value, calculated_value, currency, b_or_t, budget_type, iati_identifier]
                                    output.append(row)
                            elif value and not use_activity_recipients:
                                for transaction_recipient_code in list(transaction_recipients.keys()):
                                    transaction_recipient_percentage = transaction_recipients[transaction_recipient_code]
                                    calculated_value = value*transaction_recipient_percentage if (transaction_recipient_percentage is not None) else None
                                    if currency in self.dictionaries["ratedf"]:
                                        converted_value = convert_usd(calculated_value, year, currency, self.dictionaries["ratedf"])
                                    else:
                                        pdb.set_trace()
                                    # ["year","recipient_code","transaction_type","usd_disbursement","budget_or_transaction","budget_type","iati_identifier"]
                                    row = [year, transaction_recipient_code, transaction_type_code, converted_value, calculated_value, currency, b_or_t, budget_type, iati_identifier]
                                    output.append(row)

        return output
