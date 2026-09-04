# `receivePurposeLanguage` V1 size plan

Category `00000034` is a three-validator linear proof thread. It authenticates
one execution coordinate from the retained `NativeScripts` trace frontier; no
transaction-wide decoder or unrelated rejection predicate is reachable from
an applied script.

| Physical validator | State transition | Imported semantic engine | Maximum dynamic evidence | Fit check |
| --- | --- | --- | --- | --- |
| `step_01.main.spend` | initial thread -> bound execution coordinate | proof-thread substrate plus accepted/forced native-tx source bind | one transaction inclusion proof or one forced-leaf proof | signed max-shape accepted/forced lifecycle |
| `step_02.main.spend` | bound coordinate -> authenticated purpose/language | validation-trace proof, purpose/source/execution Merkle memberships | one retained trace membership and three maximum-depth membership paths | signed max-shape authentication transaction |
| `step_03.main.spend` | authenticated purpose/language -> permanent proof mint and thread burn | `terminal_contradiction_v1` | no additional dynamic evidence | signed terminal transaction in both directions |

The canonical state progression is
`BoundExecutionV1 -> AuthenticatedReceiveLanguageV1 -> terminal`. Step 02 binds
purpose kind `3` (receive), the selected source descriptor, its language tag,
and the execution leaf at the accused index. The decisive rule is exactly
`purpose_kind == 3 && language_tag == 3`; wrongful acceptance proves it true
and wrongful rejection proves it false. Every spend validator retains the
standard cancel arm, and the only successor is fixed by an applied script hash.
