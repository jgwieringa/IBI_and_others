import pandas as pd
from Bio import Entrez
import time, os, re, random

xlsx = os.listdir(path='xlsx')#This is where has all the excel sheets

Entrez.email = "###@###.com"# Your email adress is required by the Genbank


for i in xlsx:
    filename = i.split('_') #files are named by "genus name_species name"
    genus = filename[0]
    species = filename[1]
    print('Downloading {}'.format(i))
    df = pd.read_excel(r'xlsx\{}'.format(i))

    raw = df[["Genus", "Species", "Ingroup/Outgroup", "Loci Name", "Genbank Accession Number", "Individual #"]]
    raw_1 = list(raw.values.tolist())

    loci = df["Loci Name"]
    loci1 = loci.values.tolist()
    loci2 = list(set(loci1))


    for i in loci2:
        file = open(r"Raw_sequences\processing\{0}_{1}_{2}.fas".format(genus, species, i.replace(' ', '')), "w")
        for j in raw_1:
            if j[3] == i:
                asn = str(j[4])
                asn_clear = asn.replace(' ', '')
                num = str(j[5])
                num_clear = num.replace(' ', '')
                group = j[2]
                group_clear = group.replace(' ', '')
                g = group_clear[0].lower()

                if len(asn_clear) > 20:
                    sequence = asn_clear
                    file.write('>{0}_{1}_{2}\n'.format(num_clear, g, "NONE"))
                    file.write(sequence+'\n')
                elif len(asn_clear) < 7:
                    pass
                else:
                    handle = Entrez.efetch(db="nucleotide", id=asn_clear, rettype="fasta", retmode="text")
                    sequence = re.sub(r'>(.*?)\n', r'>{0}_{1}_{2}\n'.format(num_clear, g, asn_clear), handle.read())
                    file.write(sequence)
                    t = random.uniform(0.2,1.3)
                    time.sleep(t)
        file.close()
    print('done')