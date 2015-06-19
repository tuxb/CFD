#!/bin/sh
echo your very good!
 WORK_NAME="order"
 data="./data/error.dat"
 base=10
 tend=0.5
 len=5
 rm $data
 for((i=0;i<len;i++));
 do
     # JM=`expr $base \* $i`
    ./$WORK_NAME $(($base*(2**$i))) $tend $len
 done
        
#gnuplot plot.gp

