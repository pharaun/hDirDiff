BEGIN {
  FS = "  "
}

ARGIND == 1 {
  # remote state
  nameToHash[$2] = $1
  hashToName[$1] = $2
}

ARGIND == 2 {
  # local state, first pass (copy existing files)
  localNames[$2]=1

  if (nameToHash[$2] != $1) {
    if (hashToName[$1]) {
      print "cp", "r/" hashToName[$1], "r/" $2
      nameToHash[$2] = $1
    }
  }
}

ARGIND == 3 {
  # local state, second pass (upload new files, copy new files)
  if (nameToHash[$2] != $1) {
    if (hashToName[$1]) {
      print "cp", "r/" hashToName[$1], "r/" $2
    } else {
      print "up", "s/" $2, "r/" $2
      hashToName[$1] = $2
    }
    nameToHash[$2] = $1
  }
}

END {
  for (remoteName in nameToHash) {
    if (!localNames[remoteName]) {
      print "rm", "r/" remoteName
    }
  }
}

# gawk -f mydiff <remote> <local> <local>
