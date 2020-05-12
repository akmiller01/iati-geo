import progressbar
from lxml import etree
import datetime
import dateutil
import pdb
import json

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
        self.header = [
            "reporting_org",
            "recipient_country",
            "recipient_country_code",
            "sector_code",
            "year",
            "transaction_type",
            "usd_disbursement",
            "budget_or_transaction",
            "budget_type",
            "iati_identifier",
            "location_ref",
            "location_reach",
            "location_id_vocab",
            "location_id_code",
            "location_name",
            "location_description",
            "location_activity_description",
            "location_admin_vocab",
            "location_admin_level",
            "location_admin_code",
            "location_point_srsname",
            "location_point_pos",
            "location_exactness",
            "location_class",
            "location_feature_designation",
            "location_coordinates_lat",
            "location_coordinates_long",
            "location_coordinates_pres",
            "location_type",
            "location_gaz_ref",
            "location_gaz_entry"
            ]
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
            activity = root.xpath('iati-activity[%s]' % (i + 1))[0]
            # Capture iati identifier
            iati_identifier = default_first(activity.xpath("iati-identifier/text()"))

            child_tags = [child.tag for child in activity.getchildren()]

            secondary_reporter = default_first(activity.xpath("reporting-org/@secondary-reporter"))
            secondary_reporter = replace_default_if_none(secondary_reporter, "0")
            reporting_org = replace_default_if_none(default_first(activity.xpath("reporting-org/narrative/text()")), "")
            recipient_country = replace_default_if_none(default_first(activity.xpath("recipient-country/narrative/text()")), "")
            recipient_country_code = replace_default_if_none(default_first(activity.xpath("recipient-country/@code")), "")
            sector_code = replace_default_if_none(default_first(activity.xpath("sector/@code")), "")
            
            if "location" in child_tags:
                for location in activity.xpath("location"):
                    location_ref = replace_default_if_none(default_first(location.xpath("@ref")), "")
                    location_reach = replace_default_if_none(default_first(location.xpath("location-reach/@code")), "")
                    location_id_vocab = replace_default_if_none(default_first(location.xpath("location-id/@vocabulary")), "")
                    location_id_code = replace_default_if_none(default_first(location.xpath("location-id/@code")), "")
                    location_name = replace_default_if_none(
                        default_first(location.xpath("name/narrative/text()")),
                        replace_default_if_none(default_first(location.xpath("name/text()")), "")
                    )
                    location_description = replace_default_if_none(
                        default_first(location.xpath("description/narrative/text()")),
                        replace_default_if_none(default_first(location.xpath("description/text()")), "")
                    )
                    location_activity_description = replace_default_if_none(
                        default_first(location.xpath("activity-description/narrative/text()")),
                        replace_default_if_none(default_first(location.xpath("activity-description/text()")), "")
                    )
                    location_admin_vocab = replace_default_if_none(default_first(location.xpath("administrative/@vocabulary")), "")
                    location_admin_level = replace_default_if_none(default_first(location.xpath("administrative/@level")), "")
                    location_admin_code = replace_default_if_none(default_first(location.xpath("administrative/@code")), "")
                    location_point_srsname = replace_default_if_none(default_first(location.xpath("point/@srsName")), "")
                    location_point_pos = replace_default_if_none(default_first(location.xpath("point/pos/text()")), "")
                    location_exactness = replace_default_if_none(default_first(location.xpath("exactness/@code")), "")
                    location_class = replace_default_if_none(default_first(location.xpath("location-class/@code")), "")
                    location_feature_designation = replace_default_if_none(default_first(location.xpath("feature-designation/@code")), "")
                    location_coordinates_lat = replace_default_if_none(default_first(location.xpath("coordinates/@latitude")), "")
                    location_coordinates_long = replace_default_if_none(default_first(location.xpath("coordinates/@longitude")), "")
                    location_coordinates_pres = replace_default_if_none(default_first(location.xpath("coordinates/@precision")), "")
                    location_type = replace_default_if_none(default_first(location.xpath("location-type/@code")), "")
                    location_gaz_ref = replace_default_if_none(default_first(location.xpath("gazetteer-entry/@gazeteer-ref")), "")
                    location_gaz_entry = replace_default_if_none(default_first(location.xpath("gazetteer-entry/text()")), "")

                    defaults = {}
                    default_tags = ["default-currency"]
                    for tag in default_tags:
                        if tag in activity.attrib.keys():
                            defaults[tag] = activity.attrib[tag]
                        elif tag in child_tags:
                            defaults[tag] = default_first(activity.xpath("{}/@code".format(tag)))
                        else:
                            defaults[tag] = None
                    troublesome_publishers = ["fco", "wwf-uk"]
                    if publisher == "fco":
                        defaults["default-currency"] = "GBP"

                    if secondary_reporter == "0":
                        has_transactions = "transaction" in child_tags
                        if has_transactions:
                            transactions = activity.findall("transaction")

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
                                    budget_type = None
                                    b_or_t = "Transaction"

                                    if value:
                                        if currency in self.dictionaries["ratedf"]:
                                            converted_value = convert_usd(value, year, currency, self.dictionaries["ratedf"])
                                        else:
                                            pdb.set_trace()
                                        # ["year","transaction_type","usd_disbursement","budget_or_transaction","budget_type","iati_identifier"]
                                        row = [
                                            reporting_org,
                                            recipient_country,
                                            recipient_country_code,
                                            sector_code,
                                            year,
                                            transaction_type_code,
                                            converted_value,
                                            b_or_t,
                                            budget_type,
                                            iati_identifier,
                                            location_ref,
                                            location_reach,
                                            location_id_vocab,
                                            location_id_code,
                                            location_name,
                                            location_description,
                                            location_activity_description,
                                            location_admin_vocab,
                                            location_admin_level,
                                            location_admin_code,
                                            location_point_srsname,
                                            location_point_pos,
                                            location_exactness,
                                            location_class,
                                            location_feature_designation,
                                            location_coordinates_lat,
                                            location_coordinates_long,
                                            location_coordinates_pres,
                                            location_type,
                                            location_gaz_ref,
                                            location_gaz_entry
                                        ]
                                        output.append(row)

                            # Loop through budgets, and capture as close equivalents as we can to transactions
                            budget_output = []
                            has_budget = "budget" in child_tags
                            if has_budget:
                                budgets = activity.findall("budget")

                                for budget in budgets:
                                    transaction_type_code = None
                                    if "type" in budget.attrib.keys():
                                        budget_type = budget.attrib["type"]
                                    else:
                                        budget_type = None

                                    transaction_date = default_first(budget.xpath("period-start/@iso-date"))
                                    transaction_date_end = default_first(budget.xpath("period-end/@iso-date"))
                                    time_range = {}
                                    try:
                                        time_range["start"] = dateutil.parser.parse(transaction_date)
                                        time_range["end"] = dateutil.parser.parse(transaction_date_end)
                                    except (TypeError, ValueError) as error:
                                        time_range["start"] = None
                                        time_range["end"] = None
                                    if time_range["start"] is not None:
                                        time_range["length"] = time_range["end"]-time_range["start"]
                                        if time_range["length"] < datetime.timedelta(370):
                                            year = time_range["start"].year

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

                                            if value:
                                                if currency in self.dictionaries["ratedf"]:
                                                    converted_value = convert_usd(value, year, currency, self.dictionaries["ratedf"])
                                                else:
                                                    pdb.set_trace()
                                                # ["year","transaction_type","usd_disbursement","budget_or_transaction","budget_type","iati_identifier"]
                                                row = [
                                                    reporting_org,
                                                    recipient_country,
                                                    recipient_country_code,
                                                    sector_code,
                                                    year,
                                                    transaction_type_code,
                                                    converted_value,
                                                    b_or_t,
                                                    budget_type,
                                                    iati_identifier,
                                                    location_ref,
                                                    location_reach,
                                                    location_id_vocab,
                                                    location_id_code,
                                                    location_name,
                                                    location_description,
                                                    location_activity_description,
                                                    location_admin_vocab,
                                                    location_admin_level,
                                                    location_admin_code,
                                                    location_point_srsname,
                                                    location_point_pos,
                                                    location_exactness,
                                                    location_class,
                                                    location_feature_designation,
                                                    location_coordinates_lat,
                                                    location_coordinates_long,
                                                    location_coordinates_pres,
                                                    location_type,
                                                    location_gaz_ref,
                                                    location_gaz_entry
                                                ]
                                                meta = {"row": row, "time_range": time_range, "budget_type": budget_type}
                                                budget_output.append(meta)
                            if len(budget_output) > 1:
                                overlaps = []
                                spoiled = False
                                keep_indexes = list(range(0, len(budget_output)))
                                # All possible combinations of 2
                                for i in range(0, len(budget_output)):
                                    if i+1 < len(budget_output):
                                        for j in range(i+1, len(budget_output)):
                                            first_budget = budget_output[i]
                                            second_budget = budget_output[j]
                                            if second_budget["time_range"]["end"] <= first_budget["time_range"]["end"] and second_budget["time_range"]["end"] >= first_budget["time_range"]["start"]:
                                                overlaps.append((i, j))
                                                if i in keep_indexes:
                                                    keep_indexes.remove(i)
                                                if j in keep_indexes:
                                                    keep_indexes.remove(j)
                                            elif second_budget["time_range"]["start"] >= first_budget["time_range"]["start"] and second_budget["time_range"]["start"] <= first_budget["time_range"]["end"]:
                                                overlaps.append((i, j))
                                                if i in keep_indexes:
                                                    keep_indexes.remove(i)
                                                if j in keep_indexes:
                                                    keep_indexes.remove(j)
                                if len(overlaps) > 1:
                                    for i, j in overlaps:
                                        # If we've happened to put them back in the queue, take them out
                                        if i in keep_indexes:
                                            keep_indexes.remove(i)
                                        if j in keep_indexes:
                                            keep_indexes.remove(j)
                                        budget1 = budget_output[i]
                                        budget2 = budget_output[j]
                                        # Only keep overlaps if one is revised and one is original
                                        if budget1["budget_type"] == "1" and budget2["budget_type"] == "2":
                                            keep_indexes.append(j)
                                        elif budget1["budget_type"] == "2" and budget2["budget_type"] == "1":
                                            keep_indexes.append(i)
                                        elif budget1["budget_type"] == budget2["budget_type"]:
                                            spoiled = True
                                if not spoiled:
                                    for keep_index in keep_indexes:
                                        output.append(budget_output[keep_index]["row"])
                            elif len(budget_output) == 1:
                                # only one budget
                                output.append(budget_output[0]["row"])

        return output
