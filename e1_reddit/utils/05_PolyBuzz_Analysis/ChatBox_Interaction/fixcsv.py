import sys

input_file = "polybuzz_conversations.csv"
output_file = "fixed_data.csv"

with open(input_file, "r", encoding="utf-8") as f:
    text = f.read()

in_quotes = False
result_chars = []

for c in text:
    if c == '"':
        # Flip in_quotes whenever we see a double-quote
        in_quotes = not in_quotes
        result_chars.append(c)
    elif c == '\n':
        if in_quotes:
            # We're *inside* quotes, so replace the newline
            # e.g. with a space (or with "\n" text, if you prefer).
            result_chars.append(' ')
        else:
            # We're not in quotes, so keep the newline (end of record).
            result_chars.append('\n')
    else:
        # Normal character: just keep it
        result_chars.append(c)

fixed_text = ''.join(result_chars)

with open(output_file, "w", encoding="utf-8", newline='') as f:
    f.write(fixed_text)


# Read the csv with pandas and save ';' separated csv
import pandas as pd


df = pd.read_csv("fixed_data.csv")
df.to_csv("fixed_data.csv", sep=';', index=False)

