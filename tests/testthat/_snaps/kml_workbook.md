# kml_workbook works

    Code
      kml1 <- kml_workbook(wb = walleye_workbook, out_file = temp_kml)
    Condition
      Warning:
      Some or all values in column 'release_group' are missing values and have been assigned release date instead.

---

    Code
      kml2 <- kml_workbook(wb = walleye_workbook, kmz = TRUE, out_file = temp_kmz)
    Condition
      Warning:
      Some or all values in column 'release_group' are missing values and have been assigned release date instead.

---

    Code
      kml3 <- kml_workbook(wb = walleye_workbook, labelSize = 20, iconSize = 1,
        out_file = temp_kml_bigger)
    Condition
      Warning:
      Some or all values in column 'release_group' are missing values and have been assigned release date instead.

---

    Code
      kml4 <- kml_workbook(wb_file = temp_wb_file)
    Condition
      Warning:
      Some or all values in column 'release_group' are missing values and have been assigned release date instead.

