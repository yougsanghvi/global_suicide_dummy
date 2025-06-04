print("running script")

import cdsapi
import os

dataset = "reanalysis-era5-single-levels"
data_dir = "/global/scratch/users/yougsanghvi/"

print("before for loop")

for year in range(2016, 2021):

    print("in for loop, year: ", year)

    request = {
        "product_type": ["reanalysis"],
        "variable": ["2m_temperature"],
        "year": [str(year)],
        "month": [
            "01",
            "02",
            "03",
            "04",
            "05",
            "06",
            "07",
            "08",
            "09",
            "10",
            "11",
            "12",
        ],
        "day": [
            "01",
            "02",
            "03",
            "04",
            "05",
            "06",
            "07",
            "08",
            "09",
            "10",
            "11",
            "12",
            "13",
            "14",
            "15",
            "16",
            "17",
            "18",
            "19",
            "20",
            "21",
            "22",
            "23",
            "24",
            "25",
            "26",
            "27",
            "28",
            "29",
            "30",
            "31",
        ],
        "time": [
            "00:00",
            "01:00",
            "02:00",
            "03:00",
            "04:00",
            "05:00",
            "06:00",
            "07:00",
            "08:00",
            "09:00",
            "10:00",
            "11:00",
            "12:00",
            "13:00",
            "14:00",
            "15:00",
            "16:00",
            "17:00",
            "18:00",
            "19:00",
            "20:00",
            "21:00",
            "22:00",
            "23:00",
        ],
        "data_format": "grib",
        "download_format": "unarchived",
    }

    fn = f"era5_data_{year}.grib"
    fp = os.path.join(data_dir, fn)

    client = cdsapi.Client()

    print("downloading: ", fp)
    client.retrieve(dataset, request, fp)

