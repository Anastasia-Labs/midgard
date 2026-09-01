# Plutarch Deployment Run 2 L1 Transactions

- Network: Cardano Preprod
- Run ID: `e2e-run-plutarch2-20260831T141403Z`
- Deployment manifest ID: `10107872a37ec31a3b15ee2d2907388e345f4af540871a74320365007bbe7373`
- Result: functional E2E success
- Confirmed L1 transaction count: 42 distinct transactions

## Deployment Preparation

| Purpose | L1 transaction hash |
| --- | --- |
| Prepare hub-oracle one-shot nonce | `e9e649aa2eab2bef2d3f5e5d012772c53382a308fe8bc362aa2b376d0ef32d59` |
| Reference wallet funding 01 | `33188e157c43eca826a442749703d3036209e092f1adf16553d634daede9a988` |
| Reference wallet funding 02 | `91c47275a596707e93e995f95af4684176eb426e42fd4ae72c3c7157ce9c07aa` |
| Reference wallet funding 03 | `9b96539c7dd9783698c8d94ec6e816f34c93d08d518c820e50ab40480db4891b` |
| Reference wallet funding 04 | `ea239107d073a6afbc286de197dc0f833d197e18d972c67b68cb505142e71591` |
| Reference wallet funding 05 | `1455d21c45e1dd07ff583a0e3e9716e0ab6e96c47fb650b64655050a4c0f1b54` |
| Automatic reference wallet replenishment | `7834a3b586b41c1ca8ff42e0ddad44e92349667433beeac8cbf1aa8c5550d979` |
| Operator wallet funding | `74d982a72c5e1b621adbb20a4d85aead28f6e054c3f627ed500596d2d93b2e66` |

## Plutarch Reference Scripts

| Publication | L1 transaction hash |
| --- | --- |
| Reference script 01 | `60e1fd7ac2cad54c9dbe3889a0fda63bfa420dddc0a3b565e81b2a86a1783de5` |
| Reference script 02 | `1c5ea6841304437f9ae74503f1b946e85725755f2978ef130370ff0c67b12da0` |
| Reference script 03 | `3e1c5c119afda158ceac5c81c1112b2b787ecaf52fbb8412d686cff856178a3d` |
| Reference script 04 | `81988d83525d3eed013a9021a4b954ca54f3cc476cd4a4b924aa7161dcd76ffe` |
| Reference script 05 | `9e1afcca25073f907871bf58f3c1339cd1e84a9268158c1ab163476177aa39aa` |
| Reference script 06 | `a867f6fe4f5a5916a67ee855d9bf6cb7f7f65164bd0e5d47dd19e147657c597c` |
| Reference script 07 | `bbcf0a5adb13191c648a651287e35135897f1e578ab5cb7aa1ce9ee5ddbf7483` |
| Reference script 08 | `6768ff5b4062f87aaef44082b8ac707ea36c88012b69ec4c772d78029fabf036` |
| Reference script 09 | `6a02ca090eb0dbdd26877c3a0c1fd242b7cb857f31ff7826929d18c405fc5ce5` |
| Reference script 10 | `75fd120a266ee06e9d01426c904427cd3e6994c041520789e3cc2f1e42d5cc42` |
| Reference script 11 | `fa82c8c9ec9e67590b9032ae97c91e6d4a51fbb0d990361d5eef2f14c2b1fba2` |
| Reference script 12 | `a306024fb6854a1be89859481f26d2380ed59c97a94dcc6687369da03b20e33e` |
| Reference script 13 | `378e735e524af12628e949269e92d0a8267d177624a6f531bc4a055c047dc5e4` |
| Reference script 14 | `409c1e1f90dd4ff5f74a3c6eb958c8bad7c70242d13dcb71be7354f3eb8dee32` |

## Protocol Setup

| Purpose | L1 transaction hash |
| --- | --- |
| Initialize protocol | `10cfd8d8259567d8207d2002d67f9eee991634ca0ce7a2e852fd57848fcde1dd` |
| Register operator | `2974f66c4f7fc3f438aac17358844b450f58f203c984765085ab3583af0f8e61` |
| Activate operator | `48067849c28cadec890a12ec300cab74d2811c67fc060ef3f3f8c212e127680f` |

## Deposit And Header Commits

| Purpose | L1 transaction hash |
| --- | --- |
| Deposit | `d8fe7ed4951480e79cef77e25960c8761ed766858d4c32534fa8bc7eab405460` |
| Deposit-block header commit | `af6e482508f2c14e15467fce5b16c9a587e0186837ea1d2d5c361161a0b38940` |
| First L2-transfer-block header commit | `d43cb9e47ac79f7c7bd8992a0a5515c172656f6a84c58f38ffcb8fab3242064f` |
| Second L2-transfer-block header commit | `3fb720879a70051cc7bab63f554c7b668b3c55975b175a18b20a537d40f1ff29` |

## Deposit Block DA And Merge

Header: `9cead51480501b926918821b960ce059e267937a7c03c326a0e96bda`

| Purpose | L1 transaction hash |
| --- | --- |
| DA attestation init | `8c4aa9be3aa93728b094f1231e694db5e3aad991526004af9e4478253de4ec7c` |
| DA add signer 0 | `b1aa0271f4f740a0b597183e056816f16cd9bfbd26a47cb0ebb2a7e4b5c67608` |
| DA add signer 1 | `0cd9b187806b9ce138fdd3b6db943a670f1c7a01435791ff9865d8565ab9225f` |
| DA attestation apply | `aae77ef7e9dfce91f61e906fb223b226a0421318923c46b4e90ec32f235ffc00` |
| Merge into confirmed state | `028ecdfbb715fcf87ef588e9ec5d6adc04124fc79ba5f296d8c3d41a52fddc9e` |

## First L2 Transfer Block DA And Merge

Header: `a7cddd568eac01ba07e10215e15d697350650bb780fdba6eec82a0b1`

| Purpose | L1 transaction hash |
| --- | --- |
| DA attestation init | `6cb3f08cc1bf02461838c45f5ec45a1cbd0a7d0cd14e4b2128c6383c52fc5c5e` |
| DA add threshold signatures | `bed734eabf658af654208202e448f9672ecf2fe9cf5b7124c86941623bda217f` |
| DA attestation apply | `9a6fe755081b4ddc615d7c8e07e4f6cbf81068ff887602c7823beb74ad9ae99d` |
| Merge into confirmed state | `0485b52296769c0a4134af241eb0c2fa7c595a0f6536e21a0ce247ca87b9e5d6` |

## Second L2 Transfer Block DA And Merge

Header: `8018aa005c7f47459c7b7002e79473a27df2a1127a975768aaacba29`

| Purpose | L1 transaction hash |
| --- | --- |
| DA attestation init | `262cd49632e8ae8ad6afebfcd5c83c370bc0dc050f3c836bb4b147d947de7737` |
| DA add threshold signatures | `d117805ba0444de35202bf531e83c2b46c011988de5c769cee5591ab42217582` |
| DA attestation apply | `ef9f9b772cd01480398c55331af2e3ef1601dc85083095c00a8b803a35b1c35b` |
| Merge into confirmed state | `eccd36732e4095040f246e6d8fda4de07b1cdfbe97c40826080116893b73f908` |

## L2 Transactions

These are L2 transaction IDs and are not included in the 42-L1-transaction count:

| Transfer | L2 transaction ID | Final state |
| --- | --- | --- |
| First transfer | `a20c34a6143f106b0e154c9fbe62c61f87697399e0771a5d9c0b53e5c3aaaf56` | Confirmed-ledger finalized |
| Second transfer | `75ac27029da6a7c0acca4be72612d9309b30670d8e5691b1775dffe5dd16881c` | Confirmed-ledger finalized |

## Evidence

The complete acceptance evidence is in
`demo/midgard-node/logs/e2e-run-plutarch2-20260831T141403Z/summary.md` and
`demo/midgard-node/logs/e2e-run-plutarch2-20260831T141403Z/summary.json`.

The finalizer reported `verdict: success`, `functionalVerdict: success`, and
`nextSafeAction: none_run_complete`. Its clean-run quality gate remains failed
because the fresh deployment required pre-submission funding/configuration
retries; none of those failed attempts submitted an unknown or rejected L1
transaction.
