#!/bin/python3
import re
import sys
import os

input_files = sys.argv[1:]
if (len(input_files) == 0):
    print("Expected command-line arguments")
    print("Usages: ./generate-code-sections.py <...files>")
    sys.exit(1)

output_file = "scala/code-sections.tex"
listing_options = "breaklines"

start_section = re.compile(r'\s*(?://|#)\s+start\s+section\s+(\w+)\s*')
end_section = re.compile(r'\s*(?://|#)\s+end\s+section\s+(\w+)\s*')

starts = {}
finishes = {}

for input_file in input_files:
    with open(input_file, 'r') as f:
        for i, line in enumerate(f.readlines()):
            start_match = start_section.match(line)
            if start_match:
                starts[start_match.group(1)] = (input_file, i + 2) # minted start is inclusive and 1-based

            end_match = end_section.match(line)
            if end_match:
                finishes[end_match.group(1)] = (input_file, i) # minted end is exclusive

    if len(starts) == 0:
        print(f"No sections defined in {input_file}!")
        sys.exit(1)

def dict_zip(*dicts):
    return {k: [d[k] for d in dicts] for k in dicts[0].keys()}

output = []
for (section, positions) in dict_zip(starts, finishes).items():
    if len(positions) < 2:
        print(f"Ignoring section {section} as it does not have a start and a finish")
    else:
        [(start_file, start_line), (finish_file, finish_line)] = positions
        if start_file != finish_file:
            print(f"Start and finish for section '{section}' are in different files ({start_file}, {finish_file}")
            sys.exit(1)
        extension = os.path.splitext(start_file)[1][1:]
        output.append(f"\\newcommand{{\\{section}CodeSection}}{{\lstinputlisting[float, floatplacement=H, style={extension}, firstline={start_line}, lastline={finish_line}, {listing_options}]{{{start_file}}}}}")

with open(output_file, "w") as f:
    f.write("\n".join(output))
    print(f"Wrote {output_file}")
