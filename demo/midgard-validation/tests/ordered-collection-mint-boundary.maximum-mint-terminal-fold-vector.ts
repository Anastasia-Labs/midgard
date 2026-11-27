// The exact genuine signed-Cardano field-5/field-6 boundary. The terminal fold
// vector below is the Aiken-replayed half; these four numbers pin the policy
// cardinality and byte count the search must land on, so a silently shrunk mint
// collection can no longer satisfy the relative bounds alone.
export const MAXIMUM_MINT_POLICY_ACCEPTED_COUNT = 130;

export const MAXIMUM_MINT_POLICY_ACCEPTED_SIGNED_BYTES = 16_376;

export const MAXIMUM_MINT_POLICY_ADJACENT_COUNT = 131;

export const MAXIMUM_MINT_POLICY_ADJACENT_SIGNED_BYTES = 16_500;

export const maximumMintTerminalFoldVector = {
  fieldCommitmentHex:
    "7ba153c420ecc2fc34570ff76ea54d8b892b9b2f21baff8ae9bbaf95b7e1eab7",
  transactionIdHex:
    "7d05811c269235d01486fb1b9f5d9f08ee27dfd67f5b2f5553e98f5f63d2904c",
  transactionCommitmentHex:
    "53fab1e7ca307ed09192048fc8c4fe82b33e4cd9de40bd2e723207039d85b281",
  preWorkRootHex:
    "4a04c1b0d3ab0307fcbad5cda4985636ac30cbd52d11f229967bb21d6e027868",
  postWorkRootHex:
    "6b18ca10e35cdec5bf29736c0a43aa76b4fa32600949fce9208dde1c9fdbd798",
  encodedLengthBeforeItem: 5807,
  collectionProof: {
    fieldIndex: 5,
    itemCount: 130,
    itemIndex: 129,
    itemLength: 43,
    itemCommitmentHex:
      "534ff6685dd10a576be7dec4ecb1cf2f239a5fdcb751db98ccc1b93ebd4e5c04",
    frontier: [
      {
        height: 1,
        hashHex:
          "ce7d37cda58da9e9e61128e546de8b86657a6ba8b3412ff6b0ac9768220facc1",
      },
      {
        height: 7,
        hashHex:
          "a4e70fd3e6e67a34ff688b3ee548e87d066c538c85340620668e3b53e09d7110",
      },
    ],
    siblingHexes: [
      "7dee9561c439c28a1058042f1cccd273783fe91f56440e1b9c44e306d0aee12b",
    ],
  },
  chunkProof: {
    fieldIndex: 5,
    itemIndex: 129,
    totalLength: 43,
    chunkIndex: 0,
    chunkHex:
      "82581cffab1dd64f82b6991818c1ecc5047d52ce5d00f6fdbc5023e2980167a1494d696467617264563101",
    frontier: [
      {
        height: 0,
        hashHex:
          "ae892cdb843a795de543205f99a43ea2d0f946bcab042d2c405bd786dbad75da",
      },
    ],
    siblingHexes: [],
  },
} as const;
