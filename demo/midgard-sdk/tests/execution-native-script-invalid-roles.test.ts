import { deploymentFamilyStepNames } from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  checkExecutionNativeScriptInvalidRoleOrder,
  EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER,
} from "../src/fraud-proof/contracts/families/execution-native-script-invalid.js";
import {
  executionNativeScriptInvalidConvicts,
  type ExecutionNativeScriptInvalidFact,
} from "../src/fraud-proof/execution-native-script-invalid.js";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  type VerdictSubject,
} from "../src/fraud-proof/proof-thread-substrate.js";
import type { RejectionReason } from "../src/rejection-reason.js";

const manifestSteps = () => [
  ...deploymentFamilyStepNames("executionNativeScriptInvalid"),
];

const swapped = (first: number, second: number) => {
  const steps = manifestSteps();
  [steps[first], steps[second]] = [steps[second]!, steps[first]!];
  return steps;
};

describe("execution-native-script-invalid role order", () => {
  it("names every manifest step in the manifest's order", () => {
    expect(manifestSteps()).toHaveLength(28);
    expect(EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER).toHaveLength(28);
    expect(() =>
      checkExecutionNativeScriptInvalidRoleOrder(manifestSteps()),
    ).not.toThrow();
  });

  it("refuses a manifest that swaps the signer and evaluation steps", () => {
    const signers =
      EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER.indexOf("signers");
    const evaluate =
      EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER.indexOf("evaluate");
    expect(evaluate).toBe(signers + 1);
    expect(() =>
      checkExecutionNativeScriptInvalidRoleOrder(swapped(signers, evaluate)),
    ).toThrow(
      `execution-native-script-invalid role signers is fraudProofExecutionNativeScriptInvalidSigners, but manifest step ${signers} is fraudProofExecutionNativeScriptInvalidEvaluate`,
    );
  });

  it("refuses a manifest with a step more or less than the roles", () => {
    const steps = manifestSteps();
    for (const changed of [steps.slice(1), [...steps, steps[0]!]])
      expect(() => checkExecutionNativeScriptInvalidRoleOrder(changed)).toThrow(
        `execution-native-script-invalid has 28 roles; the manifest has ${changed.length} steps`,
      );
  });
});

const TX = "ab".repeat(32);
const ROOT = "44".repeat(32);

const forced = (reason: RejectionReason | null): VerdictSubject =>
  forcedVerdictSubject({
    transactionId: TX,
    sourceKey: { transactionId: "cd".repeat(32), outputIndex: 0n },
    rejectionReason: reason,
  });

const at = (kind: bigint, index: bigint) => ({ kind, index });
const evaluated = (
  kind: bigint,
  index: bigint,
  satisfied: boolean,
): ExecutionNativeScriptInvalidFact => ({
  Evaluated: { coordinate: at(kind, index), satisfied },
});
const none = (
  kind: bigint,
  index: bigint,
): ExecutionNativeScriptInvalidFact => ({
  NoExecution: { coordinate: at(kind, index) },
});
const recorded = (kind: bigint, index: bigint): RejectionReason => ({
  ExecutionNativeScriptFalse: { purpose_kind: kind, purpose_index: index },
});

const convicts = (
  subject: VerdictSubject,
  fact: ExecutionNativeScriptInvalidFact,
) =>
  executionNativeScriptInvalidConvicts({
    bound: {
      subject,
      compact_cbor: "",
      network_order_context: { body_network_mismatch: false },
      event_pre_root: ROOT,
    },
    fact,
  });

// The same rows are pinned on-chain in
// execution-native-script-invalid/terminal.test.ak; change both together.
describe("execution-native-script-invalid verdict twin", () => {
  it("convicts each direction exactly as the terminal does", () => {
    const accepted = acceptedVerdictSubject(TX);
    const rows: readonly [
      VerdictSubject,
      ExecutionNativeScriptInvalidFact,
      boolean,
    ][] = [
      // Wrongful acceptance: a false native script convicts.
      [accepted, evaluated(0n, 0n, false), true],
      [accepted, evaluated(3n, 4n, false), true],
      [forced(null), evaluated(1n, 2n, false), true],
      [accepted, evaluated(0n, 0n, true), false],
      [accepted, none(0n, 0n), false],
      [forced(null), evaluated(2n, 0n, true), false],
      [forced(null), none(2n, 0n), false],
      // Wrongful rejection at the recorded coordinate.
      [forced(recorded(0n, 1n)), evaluated(0n, 1n, true), true],
      [forced(recorded(0n, 1n)), none(0n, 1n), true],
      [forced(recorded(2n, 9n)), none(2n, 9n), true],
      [forced(recorded(7n, 0n)), none(7n, 0n), true],
      [forced(recorded(0n, 1n)), evaluated(0n, 1n, false), false],
      // Another coordinate, or a later recorded reason.
      [forced(recorded(0n, 5n)), evaluated(0n, 2n, false), true],
      [forced(recorded(1n, 0n)), evaluated(0n, 9n, false), true],
      [forced(recorded(0n, 5n)), evaluated(0n, 6n, false), false],
      [forced(recorded(0n, 5n)), evaluated(1n, 0n, false), false],
      [forced(recorded(0n, 5n)), evaluated(0n, 2n, true), false],
      [forced(recorded(0n, 5n)), none(0n, 2n), false],
      [forced("ScriptIntegrityHashMismatch"), evaluated(0n, 0n, false), true],
      [
        forced({
          PlutusExecutionFailed: { purpose_kind: 0n, purpose_index: 0n },
        }),
        evaluated(3n, 1n, false),
        true,
      ],
      [
        forced({ UnusedRedeemer: { redeemer_index: 0n } }),
        evaluated(0n, 0n, false),
        false,
      ],
      [
        forced({
          ExecutionNativeScriptMalformed: {
            purpose_kind: 0n,
            purpose_index: 1n,
          },
        }),
        evaluated(0n, 1n, false),
        false,
      ],
      [
        forced({
          ExecutionNativeScriptMalformed: {
            purpose_kind: 0n,
            purpose_index: 1n,
          },
        }),
        evaluated(0n, 0n, false),
        true,
      ],
    ];
    for (const [subject, fact, expected] of rows)
      expect(convicts(subject, fact)).toBe(expected);
  });

  it("refuses an acceptance carrying a reason", () => {
    const subject = {
      ...forced(recorded(0n, 0n)),
      direction: acceptedVerdictSubject(TX).direction,
    };
    expect(() => convicts(subject, evaluated(0n, 0n, true))).toThrow();
  });
});
