# Pick a place to do the installation, e.g.
d=/usr/bin/smlnj     # or whatever you like
mkdir $d
cd $d

#- Download config.tgz, e.g.
v=110.74               # or whatever is the version you desire
wget http://smlnj.cs.uchicago.edu/dist/working/$v/config.tgz

#- Gunzip and untar, e.g.
gunzip <config.tgz | tar xf -

# This creates a subdirectory "config"
#- Edit the "config/targets" file to your taste.  If you leave this
#  file alone, you end up with a minimal installation.

#- Run the installer, it will automatically fetch all other necessary
#  tarballs:
config/install.sh
