BEGIN {
  FS = "[ :;,]"
}

{
  delete maximum 
  for(i=1; i<=NF; i++) {
    if (maximum[$i] < previous_cell) maximum[$i] = previous_cell
    if ($i ~ /[0-9]+/) previous_cell = $i
  }
}
{
  sum+=maximum["red"] * maximum["blue"] * maximum["green"]
}

END {
  print sum
}
