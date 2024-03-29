# FRISBEE THROW Readme

This repository contains the dataset(s) and script(s) to analyze the throwing distance of different types of discs.

This data was compiled by Sejin Kim and Josh Katz (Kenyon College), and includes 30 observations of disc or frisbee throws with different disc types and throw types, including a disc golf disc and a USA Ultimate frisbee, and using the backhand, the flick, and the hammer throws. All data will be collected on 10 November 2019, in order to keep noise due to changing environmental factors, such as humidity or temperature, to a minimum.

The data analysis will be done using **R** and the R front-end **RStudio**.

## Summary of Variables

|Header|Definition|
|--|--|
|`ThrowID`|The serial number of the throw, from 00 to UNDEFINED, inclusive.|
|`DISC`| The type of disk that was used, in English, categorical. It includes the options (disc) and (frisbee).|
|`DISCNUM`|A numerical encoding of the type of disk, where (0 = disc) and (1 = frisbee).|
|`TYPE`|The type of throw, in English, categorical. It includes the options (backhand), (flick), or (hammer).|
|`TYPENUM`|A numerical encoding of the type of throw, where (0 = backhand), (1 = flick), and (2 = hammer).|
|`TOTALDISTANCE`|The distance that the throw went, measured to, approximately, the centimeter. Encoded in meters, with float 3.|
|`YYARDS`|The forward distance that the throw went, measured in yards, with float 1.|
|`YMETERS`|The forward distance that the throw went, converted to meters, using the conversion factor 0.9144, with float 3.|
|`XMETERS`|The distance that the throw drifted from center, measured in meters, with float 2.|
|`BIN_FLICK`|A binary variable where if the throw was of type "flick," `BIN_FLICK` = 1, otherwise it is 0.|
|`BIN_HAMMER`|A binary variable where if the throw was of type "hammer," `BIN_HAMMER` = 1, otherwise it is 0.|


## Notes

 - It is worth noting that to properly eliminate unnecessary run-to-run variation or noise, we should use some sort of throwing machine. However, to get such a machine that could throw all three types would be prohibitively expensive (if one even exists), and thus, Josh will play the role of throwing machine. In order to prevent certain throws from being ranked lower due to exhaustion, the next throw will be determined by a random number generator, using the R script `sample(1:30)`, and assigning each throw a unique ID.
- We intend on making the data and analysis to be made open source so that if someone wishes to view the raw dataset and any techniques used, they may. The project will be hosted here.
