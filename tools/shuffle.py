import sh

sh.shuf("examples/data_sets/GDPR/gdpr.quotefix.csv", out="examples/data_sets/GDPR/gdpr.quotefix.shuffled.csv")