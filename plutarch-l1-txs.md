# Plutarch Deployment L1 Transactions

- Network: Cardano Preprod
- Run ID: `e2e-run-plutarch-20260831T084639Z`
- Deployment manifest ID: `3390ad0f65cf7d24c48b2ba54f1d75fd78267aef8517e6af84d5d64df34b5ee3`
- Result: functional E2E success after interrupted-run recovery

This file lists the 31 distinct confirmed Cardano L1 transactions associated
with the Plutarch deployment and its functional E2E run. L2 transaction IDs
are not included.

## Deployment Preparation

| Purpose | L1 transaction hash |
| --- | --- |
| Retire previous reference scripts | `c02875b3f543f2edb524e94b44e09909bad59cb44b2abc439b2c58d742e5153f` |
| Prepare hub-oracle nonce | `89b3e556079fd41ba623a4308458d73c852fad72f27a7fd928063643732962c0` |

## Plutarch Reference Scripts

| Publication | L1 transaction hash |
| --- | --- |
| Reference script 01 | `4b50f90b2b86329a20f4471c86a294c68a9fd1b697936d7ced7bad165cb45972` |
| Reference script 02 | `457116b05a92ca535c35b32b97a0e736b6114bec6b9f51bccd7af2b672cdcda4` |
| Reference script 03 | `55b3cfe142e784bad796a4f1b6d9caa712e3c0a4bcd3f86a81978ace6552b961` |
| Reference script 04 | `5b970f1d940475ffb538088926d31e95f2bfb8517b005bc3a986c81019b84d40` |
| Reference script 05 | `3a4841b2b35c4d1f71164c7dfb0a83f6b79a8db430258d2f3ad7e0360d1ad153` |
| Reference script 06 | `a60bca5ee72f64ba833d562e6cb57d12def82879062e455fab57703a698c4922` |
| Reference script 07 | `7291ff496774896b2969e8024f5913cd9a3d96214045b91f5161698ae1aa51bd` |
| Reference script 08 | `d304e7d262c451f502017beeb7ee8e2ebf9413c8e27d7f6150bd6eebb23ae6bc` |
| Reference script 09 | `31440ca58101bd975646c2b414ee2e003efb46b6eadc7bc26028811925b7438e` |
| Reference script 10 | `e7d9da346c361cec5984979da9d32629322976726dc38b164e5db5ebd7bdb950` |
| Reference script 11 | `8df86d0543cdf46d574a8fe389997869ed92dfe93b653da31ab132bce606b2fd` |
| Reference script 12 | `14163c398f119c3559c1cb2ab26befc99339a653ae4e5ff5cf479866807c516d` |
| Reference script 13 | `8464c3b7d724793907e42a7a230d4c5efec4a2b0a98be5286fd44665cd55a8f7` |
| Reference script 14 | `6c87bdb82cc69eabdc23915739cde34c2b96bf7ceff1501d4f911a606ae43fda` |
| Reference script 15 | `bb5878ae8a793ee262632a3ebeae79c1d44334f4364ef5624e3cc5773999c81a` |

## Protocol Setup

| Purpose | L1 transaction hash |
| --- | --- |
| Initialize protocol | `5198c994497cdf2443de58f087200d7cd4086c45eb5ca79973c63a406521a674` |
| Register operator | `de980e7e28d0ad30bae9097d219a3351a83207f19fcf864b599ae04dc5b60b09` |
| Activate operator | `ddad04b10ae796555f92663e0fedb84a619cabd6d5e5035a8f3d19289d9e5685` |

## Deposit And Header Commits

| Purpose | L1 transaction hash |
| --- | --- |
| Deposit | `b897f7a871308b3c850c445fb7dbef8a24bf66e1528b46fce5c9b807c8ae4d5b` |
| Deposit-block header commit | `7ea2ecc7b1b1d983dc9043e377bf77d327bbe9a40f871e86b1ffece69ef71cb5` |
| L2-transfer-block header commit | `ed3492d334751ab2d2ecca68fee43d2ef5aac814893637b2a6ac6c99ac3071d5` |

## Deposit Block DA And Merge

Header: `afc5496bcec4dd64a8880c9d7966ad5db41ec9200d21e3570d012da9`

| Purpose | L1 transaction hash |
| --- | --- |
| DA attestation init | `4a6fe210c58911c0539e4bd34fdfc857b77f1d7f38416a55a9c69db6ee8b8bfc` |
| DA add signatures | `d6f3433ee72748e7a95512c0ca1d047f94c741860a7a20769351587093724863` |
| DA attestation apply | `686e417cf43e0da8a22004b31738fc416af78199043d390323624b9e90df150b` |
| Merge into confirmed state | `a1921a56ea3bb8c4367a4c38692cc8d9be7af9601288fe2f7f59b73af9d6b95d` |

## L2 Transfer Block DA And Merge

Header: `973db84cfbb23f1111c4a3361813af80c136ce2715ef6a905a69020a`

| Purpose | L1 transaction hash |
| --- | --- |
| DA attestation init | `32a488ae81b07cdbb0f3b667e6f4279d53b082bc895f61e8b6ce72d650a46cc5` |
| DA add signatures | `4b95bf42e57a5ac9b4223ad323bcfd691e680652406c69c944603981819f0ead` |
| DA attestation apply | `99d65a1e2eed8d9fe938444cd0ac14a2a631baa298eddbb9b9c67bc48cf2eae5` |
| Merge into confirmed state | `bab0afac2fb40f08a170204d276e90ee928d435de113a469678496681fe34712` |

## Evidence

The recovered run's complete acceptance evidence is in
`demo/midgard-node/logs/e2e-run-plutarch-20260831T084639Z/summary.md` and
`demo/midgard-node/logs/e2e-run-plutarch-20260831T084639Z/summary.json`.
