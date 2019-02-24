# ZwdCSV
CSV Parser and Generator for ABAP

The Parser should respect [RFC 4180](https://tools.ietf.org/html/rfc4180), especially end-of-line characters inside of cells. Because of this, the entire CSV string is needed for correct parsing.

Class ZCL_WD_CSV contains Methods PARSE_STRING and GENERATE_STRING for directly handling csv strings.  
Class ZCL_WD_CSV_FILE contains Methods for dealing with CSV files (reading and writing files from/to application server and frontend).

UTF-8 Encoding for methods of class ZCL_WD_CSV_FILE is assumed. 
