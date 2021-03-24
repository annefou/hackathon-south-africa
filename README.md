# hackathon-south-africa

Hackathon on software testing with Team from South Africa

It includes the Fortran main program interpolate_sub.f90.

Input files:
- WRF_TMIN-TMAX.dat  (longitude, latitude, temperature min and max
- MaxTemp.txt: station latitude and longitude, date and temperature max
- MinTemp.txt: same as MaxTemp.txt but temperature min

Try it out on binder: [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/annefou/hackathon-south-africa/HEAD)

- console.cloud.google.com seems to be free and we can run 
    - open console and clone repository

Try out to update the console:

```
sudo apt-get update
sudo apt-get install -y gcc gfortran

```

Get the code:

```
git clone https://github.com/annefou/hackathon-south-africa
cd hackathon-south-africa
```
Then compile and run the code:


```
gfortran interpolate_sub.f90 -o interpolate.x

```

And to run

```
./interpolate.x
