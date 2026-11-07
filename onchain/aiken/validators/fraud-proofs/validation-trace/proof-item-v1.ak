validator main {
  else(_) {
    // Complete proof items are append-only L1 evidence. A reference input can
    // resolve the inline datum for every dependent dispute without copying
    // the item bytes into the consuming transaction.
    fail
  }
}
