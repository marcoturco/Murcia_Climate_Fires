#!/bin/bash

cd /diskonfire/FWI_T_EU


for file in *_int.nc; do
  base=$(basename $file _int.nc)
  cdo sellonlatbox,-2.5,-0.5,37.0,39.0 $file ${base}_Murcia.nc
  cdo seasmean ${base}_Murcia.nc ${base}_Murcia_seasonal.nc
  cdo gtc,50 ${base}_Murcia.nc ${base}_Murcia_gt50.nc
  cdo monsum ${base}_Murcia_gt50.nc ${base}_Murcia_monthly_days_gt50.nc
done
