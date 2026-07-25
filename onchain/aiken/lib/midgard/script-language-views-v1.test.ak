use midgard/script_language_views_v1

const redeemer_hash =
  #"1111111111111111111111111111111111111111111111111111111111111111"

test script_integrity_language_view_vectors_match_typescript() {
  and {
    script_language_views_v1.expected_script_integrity_hash(redeemer_hash, 0) == #"01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53",
    script_language_views_v1.expected_script_integrity_hash(redeemer_hash, 1) == #"d7239eb1bd8b7376dedfbf7e6201815b225c023d11c975cd99d25d5236b199a1",
    script_language_views_v1.expected_script_integrity_hash(redeemer_hash, 2) == #"71201d25ea11e4104eda108782a7d67b37b4ae97df6dc3258b06d9c98e58bbcb",
    script_language_views_v1.expected_script_integrity_hash(redeemer_hash, 3) == #"6d49b4f24c60bec1cb34a2538278252059ec0601b7f675ef73fe2b48e24317d8",
  }
}
