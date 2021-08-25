# Running `populations` on Hyak

#### M. Fisher 
#### 2021-08-25


## Description
Prepare Migrate input files using Stacks output. 

(1) Access the MERlab Hyak node, (2) download Stacks v1.44, and (3) re-run `populations` to produce a RAD loci FASTA file. 

Resources: [Nam Pho's Tutorial](https://github.com/merlab-uw/Klone/blob/main/Using_Hyak_tutorial_by_Nam_Pho.md)

<br>
<br>

## 1: Access the MERlab Hyak Node

This is my first time entering Hyak, and I'm doing it remotely from home. First, I connected to the UW Campus Traffic Network using the Big-IP client. Then, from the **git bash** terminal:

```
ssh mfisher5@klone.hyak.uw.edu    # login with UW net ID

> ECDSA key fingerprint is <>.
> Are you sure you want to continue connecting (yes/no)? 
yes
> Warning: Permanently added 'klone.hyak.uw.edu' to the list of known hosts.
> Password:

my-UW-password                     # enter UW net ID password

> Duo two-factor login for mfisher5
>
> Enter a passcode or select one of the following options:
>
>  1. Duo Push
>  2. Phone call
>
> Passcode or option (1-2):

2                                   # opt for phone call 


cd /gscratch/merlab/                # go to MERlab's home directory
ls
> ctarpey  elpetrou  genomes  mfisher  nclowell  pollock_wgs  singularity_sif  software  usage_report.txt

cd mfisher
ls
> stacks_b8_wgenome
```


Carolyn copied in all of my intermediate Stacks files that I'll need to run populations. 

## 2: Download and Install Stacks

I went looking for older versions of Stacks, and found that the following are available as downloads on the [*Frequently Asked Questions*](https://catchenlab.life.illinois.edu/stacks/faq.php#prev) webpage: 

- stacks-1.19.tar.gz
- stacks-1.23.tar.gz
- stacks-1.27.tar.gz
- stacks-1.29.tar.gz
- stacks-1.37.tar.gz
- This is the final version 1 release: stacks-1.48.tar.gz

v1.48 is the closest I can get to v1.44, so I right clicked on the download link and selected *Inspect*. From there, I got this download link: 

`<a href="http://catchenlab.life.illinois.edu/stacks/source/stacks-1.48.tar.gz">stacks-1.48.tar.gz</a>`


```
[ctarpey@klone1 mfisher]$ wget http://catchenlab.life.illinois.edu/stacks/source/stacks-1.48.tar.gz
--2021-08-25 15:15:30--  http://catchenlab.life.illinois.edu/stacks/source/stacks-1.48.tar.gz
Resolving catchenlab.life.illinois.edu (catchenlab.life.illinois.edu)... 130.126.48.94
Connecting to catchenlab.life.illinois.edu (catchenlab.life.illinois.edu)|130.126.48.94|:80... connected.
HTTP request sent, awaiting response... 200 OK
Length: 895195 (874K) [application/x-gzip]
Saving to: ‘stacks-1.48.tar.gz’

stacks-1.48.tar.gz  100%[=================>] 874.21K  3.84MB/s    in 0.2s

2021-08-25 15:15:30 (3.84 MB/s) - ‘stacks-1.48.tar.gz’ saved [895195/895195]
```
 Unzipping the tar file, moving into the directory, compiling and making the program: 
```
tar xzvf stacks-1.48.tar.gz
cd stacks-1.48 
./configure
make 
```


