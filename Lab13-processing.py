import gzip

with gzip.open('./data/stateoftheunion1790-2017.txt.gz', "rt") as fid:
    # Header
    for linenum, line in enumerate(fid):
        if 'CONTENTS' in line:
            next(fid)
            break

    # Meta-data
    sname = []
    pres = []
    year = []
    for linenum, line in enumerate(fid):
        if '***' in line:
            break

        sname.append(line.strip().replace(',', ''))
        pres.append(line.split(',')[0].strip())
        year.append(line.split(',')[-1].strip())

    # Speech data
    rawdat = fid.read()

# Clean speech data
speeches = rawdat.split('***')
speeches = speeches[:-2]
for i, speech in enumerate(speeches):
    speeches[i] = '  '.join(speech.split('\n')[5:])

# Save
with open('./data/sotu.csv', 'w') as f:
    f.write('\n'.join(speeches))

with open('./data/sotu_meta.csv', 'w') as f:
    for dat in zip(sname, pres, year):
        f.write(', '.join(dat) + '\n')
