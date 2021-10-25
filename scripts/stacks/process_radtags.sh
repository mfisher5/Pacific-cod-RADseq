#!/bin/bash

cd /mnt/hgfs/Shared\ Drive\ D/Pacific\ cod/DataAnalysis/PCod-Korea-repo/scripts

process_radtags -f /media/mfisher5/New\ Volume/Mary/Raw\ Data/1275_S13_L008_R1_001.fastq.gz \
-i gzfastq \
-y gzfastq \
-o samplesT142 \
-b scripts/barcodes_L5.txt \
-e sbfI \
-E phred33 \
-r -c -q -t 142 2>> samplesT142/process_radtags_outputL5.txt