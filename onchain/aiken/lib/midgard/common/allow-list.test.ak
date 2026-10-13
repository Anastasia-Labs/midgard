use midgard/common/allow_list.{admits, admitted_or}

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

test admitted_or_signs_as_the_first_listed_key() {
  and {
    admitted_or(None, other) == other,
    admitted_or(Some([listed, other]), other) == listed,
    admits(Some([listed]), admitted_or(Some([listed]), other)),
  }
}
