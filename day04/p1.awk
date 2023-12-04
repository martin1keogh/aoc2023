NR == 1 {
  for (i=3; i<=NF; i++) if ($i ~ /\|/) PIPE=i
}

{
  res = 0;

  for (i=3; i<PIPE; i++) {
    seen[NR " " $i] = 1;
  }

  for (i=PIPE+1; i<=NF; i++) {
    if (seen[NR " " $i]) {
      res = (res < 1 ? 1 : res * 2)
    }
  }

  sum += res
}

END {
  print sum
}
