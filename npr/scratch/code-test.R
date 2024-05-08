library(data.table)
set.seed(123)
dd1 <- data.table(date = as.Date("2023-01-01") + sample(1:20, 6),
                  key2 = c("A", "B", "C", "D", "E", "F"),
                  value1 = 1:6,
                  value2 = sample(6:11, 6, replace = T))

dd2 <- data.table(date = as.Date("2023-01-01") + sample(1:20, 6),
                  key2 = c("A", "B", "C", "D", "E", "F"),
                  value2 = sample(6:11, 6, replace = T))

dd1
dd2

kk2 <- c("value2", "value1")
kk <- "value2"
dd1[, .N, keyby = c1, env = list(c1 = as.list(kk2))] #mget
dd1[, lapply(c1, sum), env = list(c1 = as.list(setNames(nm = kk2)))]
dd2[, .N, keyby = c1, env = list(c1 = kk)] #get


# Set keys for rolling join
setkey(dd1, date, key2)
setkey(dd2, date, key2)

# Specify a rolling join with a 2-day difference
result <- dd2[dd1, roll = 2, rollends = c(FALSE, FALSE), on = .(date, key2), nomatch = 0L]

dd2[dd1, on = .(key2, date), roll = 4]

dd2[dd1, on = .(key2, date), roll = 4, nomatch = 0L]
dd2[dd1, on = .(key2, date), roll = 4, rollends = c(FALSE, FALSE), nomatch = 0L]

## roll = 2 specifies that the rolling join should consider observations within a 2-day difference.
## rollends = c(FALSE, FALSE) specifies that the rolling join should not consider the ends of the range, ensuring that only observations within the specified range are considered.
## on = .(date, key2) specifies the columns to join on.
## nomatch = 0L ensures that only matching rows are returned.

## Example 2
# Example datasets
set.seed(123)
dtt1 <- data.table(date = as.Date("2023-01-01") + sample(1:20, 5),
                   key2 = c("A", "B", "C", "D", "E"),
                   value1 = 1:5)
dtt2 <- data.table(date = as.Date("2023-01-01") + sample(1:20, 5),
                   key2 = c("A", "B", "C", "D", "E"),
                   value2 = 6:10)

dtt1
dtt2

# Set keys for non-equi join
setkey(dtt1, date, key2)
setkey(dtt2, date, key2)
dtt1[dtt2, on = .(key2, date), roll = -8]

# Merge datasets based on dates with a range of 3 days difference and matching key2
result <- dtt1[dtt2, on = .(date >= date - 3, date <= date + 3, key2)]


## Check diff from previous line
# Example data table
dt <- data.table(date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-05", "2023-01-08")),
                 value = c(1, 2, 3, 4))

# Shift the 'date' column by one row and calculate the difference
dt[, diff_date := date - shift(date, fill = NA),]
dt

# Example data tables
exp1 <- data.table(id = c(1, 2, 3, 2), value1 = c("A", "B", "C", "S"))
exp2 <- data.table(id = c(1, 1, 2, 5), value2 = c("X", "Y", "Z", "K"))

exp1
exp2

# Inner join with 'mult' argument
exp1[exp2, .(id, value1, value2), on = "id", mult = "all"]
exp1[exp2, .(id, value1, value2), on = "id", mult = "last"]
exp1[exp2, .(id, value1, value2), on = "id", mult = "first"]


tdt <- data.table(
  col1 = c("A", "B", "C", "A"),
  col2 = c(1, 2, 3, 1),
  col3 = c("X", "Y", "Z", "X")
)

tdt[, exID := paste(col1, col2, col3, sep = "_")]
tdt
