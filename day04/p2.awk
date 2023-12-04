NR == 1 {
  for (i=3; i<=NF; i++) if ($i ~ /\|/) PIPE=i
}

{
  num_reads[NR] += 1;
  res = 0;

  for (i=3; i<PIPE; i++) {
    seen[NR " " $i] = 1;
  }

  for (i=PIPE+1; i<=NF; i++) {
    if (seen[NR " " $i]) {
      res += 1
    }
  }

  for (i=1; i<=res; i++) {
    num_reads[NR+i] += num_reads[NR]
  }

  sum += num_reads[NR]
}

END {
  print sum
}
