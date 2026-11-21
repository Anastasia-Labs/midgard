use midgard/script_language_views_v1

const redeemer_hash =
  #"1111111111111111111111111111111111111111111111111111111111111111"

test script_integrity_language_view_vectors_match_typescript() {
  and {
    script_language_views_v1.expected_script_integrity_hash(redeemer_hash, 0) == #"01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53",
    script_language_views_v1.expected_script_integrity_hash(redeemer_hash, 1) == #"b37aff1034628d20a78418d2c2cf20796ac08c83467b6f8558e6cea542de4c26",
    script_language_views_v1.expected_script_integrity_hash(redeemer_hash, 2) == #"74c606847a40bed5f147cd6cca208fa1b0db6a130f7d86d7c478fd5838d6d878",
    script_language_views_v1.expected_script_integrity_hash(redeemer_hash, 3) == #"ceab70e09654e1f35ddf1566b15861055566fda429e056107de070c58c8b3991",
  }
}

test script_integrity_rejects_a_short_redeemer_hash() fail {
  script_language_views_v1.expected_script_integrity_hash(
    #"11111111111111111111111111111111111111111111111111111111111111",
    1,
  ) == #""
}

test script_integrity_rejects_a_negative_language_bitmap() fail {
  script_language_views_v1.expected_script_integrity_hash(redeemer_hash, -1) == #""
}

test script_integrity_rejects_an_unknown_language_bitmap() fail {
  script_language_views_v1.expected_script_integrity_hash(redeemer_hash, 4) == #""
}
