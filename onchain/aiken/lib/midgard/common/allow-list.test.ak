use midgard/common/allow_list.{admits}

const listed = #"11111111111111111111111111111111111111111111111111111111"

const other = #"22222222222222222222222222222222222222222222222222222222"

test admits_every_key_without_a_list() {
  admits(None, listed) && admits(None, other)
}

test admits_a_listed_key() {
  admits(Some([other, listed]), listed)
}

test refuses_an_unlisted_key() {
  !admits(Some([listed]), other)
}

test refuses_every_key_under_an_empty_list() {
  !admits(Some([]), listed)
}
