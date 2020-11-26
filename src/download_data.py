"""Downloads csv data from the web to a local filepath as a csv.
Usage: download_data.py --url=<url> --path=<path> 
 
Options:
<url>               URL from where to download the data (must be in standard csv format)
<path>          Path (including filename) of where to locally write the file
"""

import os
import pandas as pd
from docopt import docopt

opt = docopt(__doc__)


def main(url, path):
    data = pd.read_csv(url, header=None)
    try:
        data.to_csv(path, index=False)
    except:
        os.makedirs(os.path.dirname(path))
        data.to_csv(path, index=False)


if __name__ == "__main__":
    main(opt["--url"], opt["--path"])
