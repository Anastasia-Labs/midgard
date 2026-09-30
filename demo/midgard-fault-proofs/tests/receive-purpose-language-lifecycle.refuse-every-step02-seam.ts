import { expect } from "vitest";

import { type ReceivePurposeLanguageAuthentication } from "../src/receive-purpose-language/submit-step-02.js";
import {
  coverage,
  type Harness,
  mutate,
  progress,
} from "./receive-purpose-language-lifecycle.make-harness.js";
import { submitReceiveStep02Raw } from "./support/receive-purpose-language-emulator.js";
import { expectOnchainRefusal } from "./support/submit-init-emulator-shared.js";

/** Every step-02 authentication seam, mutated one at a time against a bound thread. */
export const refuseEveryStep02Seam = async (
  h: Harness,
  threadOutRef: string,
  authentication: ReceivePurposeLanguageAuthentication,
) => {
  const attempt = async (
    seam: string,
    mutated: ReceivePurposeLanguageAuthentication,
  ) => {
    progress(`step-02 refusal: ${seam}`);
    await expectOnchainRefusal(
      async () =>
        await submitReceiveStep02Raw({
          ...h.common(threadOutRef, 1),
          authentication: mutated,
        }),
    );
    coverage.seams.add(seam);
  };
  const membership = authentication.trace_membership;
  await attempt(
    "validation_traces_root",
    mutate(authentication, {
      trace_membership: { ...membership, root: "ff".repeat(32) },
    }),
  );
  await attempt(
    "trace_descriptor",
    mutate(authentication, {
      trace_membership: {
        ...membership,
        value: {
          ...membership.value,
          step_count: membership.value.step_count + 1n,
        },
      },
    }),
  );
  await attempt(
    "subject_event_key",
    mutate(authentication, {
      trace_membership: {
        ...membership,
        key: { L2TransactionEventKey: { tx_id: "aa".repeat(32) } },
      },
    }),
  );
  await attempt(
    "machine_state",
    mutate(authentication, {
      machine_state: {
        ...authentication.machine_state,
        prior_ledger_root: "ee".repeat(32),
      },
    }),
  );
  expect(authentication.trace_proof.siblings.length).toBeGreaterThan(0);
  await attempt(
    "trace_proof",
    mutate(authentication, {
      trace_proof: {
        ...authentication.trace_proof,
        siblings: [
          "dd".repeat(32),
          ...authentication.trace_proof.siblings.slice(1),
        ],
      },
    }),
  );
  await attempt(
    "native_control",
    mutate(authentication, {
      control: {
        ...authentication.control,
        purpose_peaks: authentication.control.purpose_peaks.map((peak) => ({
          ...peak,
          hash: "cc".repeat(32),
        })),
      },
    }),
  );
  await attempt(
    "purpose_item",
    mutate(authentication, { script_hash: "bb".repeat(28) }),
  );
  await attempt(
    "source_language",
    mutate(authentication, {
      language_tag: authentication.language_tag === 3n ? 0n : 3n,
    }),
  );
  expect(authentication.execution_siblings.length).toBeGreaterThan(0);
  await attempt(
    "execution_membership",
    mutate(authentication, {
      execution_siblings: [
        "99".repeat(32),
        ...authentication.execution_siblings.slice(1),
      ],
    }),
  );
};
