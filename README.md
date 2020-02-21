# IATIFlat
Some Python code meant to flatten IATI XML into something that resembles the OECD DAC CRS.

## Operation
main.py details my current use-case. But generally iatiflat can be used like so.

```
#Import pandas and lxml etree
import pandas as pd
from lxml import etree

#Import everything in iatiflat
from iatiflat2 import *

#Initialize flattener class
iatiflat = IatiFlat()

#Parse XML from file
filepath = "C:\\Data\\Worldbank\\worldbank-89.xml"
root = etree.parse(filepath).getroot()

#Flatten to array of arrays
output = iatiflat.flatten_activities(root)

#Write to CSV
data = pd.DataFrame(output)
data.columns = iatiflat.header
data.to_csv("C:\\Data\\worldbank-89-output.csv",index=False,encoding="utf-8")
```

## Internal logic
The flatten_activities function within iatiflat follows the following logical rules
* Attempt to establish a sectoral and recipient percentage breakdown
* Collect meta-data from a transaction, a budget, or the defaults from the parent activity
* For each unique combination of sector and recipient found in the breakdown, output one row of data
* That row of data will contain meta-data associated with its parent transaction/budget, along with a value divided by the sector and recipient breakdown
* Ignore transactions/budgets where:
    * Both sector and recipient percentages cannot be established
    * The transaction type is not "D" or "E" for version 1.X or 3 or 4 for version 2.X (to align with the CRS)
    * The sector vocabulary is not one of the following "", "1", "2", "DAC", or "DAC-3"
    * The value is 0 or missing
