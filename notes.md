Check NAs and "?'s" for full columns
Look into the almost entirely incomplete columns. 
    - Are these columns actually levels of a factor?
    - If so we can combine them, or keep them as dummy variables
How can we reduce the number of levels of factors?
    - This is important for the yes/no with extra notes. 
        - Should this info be truncated or converted into extra levels?
Are the empty columns ACTUALLY empty? 
    - It's possible a missing value is actually just a "no" for example. Depends on context
    
---
Next steps:
    - Start with the MOST and LEAST complete columns. 
    - Do a bit more digging into those. 
    
Later on:
    - Are there columns where missing values tend to be grouped for a row?
    - In other words, are missing values correlated at all between sparse factors. 
    - It's possible a row missing a column may be indicitive of missing values in other columns.

---

## Sep 7th

 Column Diameter: is blank the same as NA?

Check whether the atrium is adjacent to a porticod walkway
### Random notes
Correlation between column height and column style?
NA IN the spreadsheet itself tends to be a more powerful indicator than a blank cell. 

### Next Steps
Get a top 20 list of variable candidates
