use aiken/merkle_patricia_forestry as mpf

pub type Vector {
  before: ByteArray,
  after: ByteArray,
  key: ByteArray,
  value: ByteArray,
  proof: mpf.Proof,
}

fn fork_opening() -> mpf.Proof {
  [
    mpf.Branch {
      skip: 0,
      neighbors: #"bd3871c02105e5ec24751ba8fb1a5e6d285cdcc8399993a0ca82b26f5ae179d65fed1c2681e29bbb4baed7e7d2a0f618637b48308f351ef786980f3dcdf001205318b031fecedfbfb987a7db1e2fcbb1c1d1c82dbf31e099558ddab0e96e3d38c73adabe2c5d1f9741c04a9dc2e856e29ecb6b96346905f7fb7b266b7865c857",
    },
    mpf.Branch {
      skip: 0,
      neighbors: #"42a0d1b9e50273451d2a93f195a000843d6a0c114672d0b5fadbf8fce300fb90b6be92c2f492c9bce1b8ecac7b530487537b2f448a80be834ad7a21df127a5468fe7d543b2434538a0c6d78fae96ed7f767ed36752ce7c1f54242d57eeaaf059cec5360efb71d228292cdf2b52f551f199b32bda593c4a4745a9b94aa17f8c9a",
    },
    mpf.Fork {
      skip: 1,
      neighbor: mpf.Neighbor {
        nibble: 8,
        prefix: #"",
        root: #"2f425ececd6ad91ad57238470324c4623ff0bcd1b51f6393ce0105874e651ccb",
      },
    },
  ]
}

pub fn fork() -> Vector {
  Vector {
    before: #"a3cb6c7cc11c8ab91629b7fa17089818c9751c5a9a0f256b0449acc17a6fb5b7",
    after: #"aae9218b732ed0dd511191290953ea78b87bf1d3a9e53fa0d2753fe678c9ba9b",
    key: #"abababababababababababababababababababababababababababab001f",
    value: #"01",
    proof: fork_opening(),
  }
}

fn leaf_opening() -> mpf.Proof {
  [
    mpf.Leaf {
      skip: 1,
      key: #"9706e52f00c679e548b5155af5026f5af4130d7a15c990a791fff8d652c464f5",
      value: #"1111111111111111111111111111111111111111111111111111111111111111",
    },
    mpf.Branch {
      skip: 0,
      neighbors: #"2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222",
    },
  ]
}

pub fn leaf() -> Vector {
  Vector {
    before: #"88e28232999483e47f2779227d77d46358aad506223f4de36808d08fc57f11c4",
    after: #"3e090fbdb93ef5cbeb10616c4f7238e36149d89d9974620230c304e36f7a517b",
    key: #"abcd",
    value: #"01",
    proof: leaf_opening(),
  }
}

pub const dropped_fork_prefix_root =
  #"c8aa9e6b96398745d8c5ec972fc6a0f3baaf7cbe154b9fcd5ae642d8ffa82f36"
