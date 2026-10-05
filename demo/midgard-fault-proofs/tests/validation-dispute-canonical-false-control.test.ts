import { buildValidationDisputeEvidenceBundle } from "@al-ft/midgard-validation";
import { expect, it } from "vitest";

import { runForcedValidationDisputeScenario } from "./support/emulator/dispute-scenario.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { buildInstalledCanonicalFixture } from "./support/installed-canonical-fixture.js";

it("refuses a manual false challenger against an honest maximum Certified output", async () => {
  // The installed public authority refuses dishonest evidence before opening.
  // This separate manual protocol control reaches the actual compiled predicate.
  const failure = await expectOnchainRefusal(
    () =>
      runForcedValidationDisputeScenario(
        async (input) => {
          const fixture = await buildInstalledCanonicalFixture({
            ...input,
            dishonestChallenger: true,
          });
          const membership = fixture.claim.source_membership;
          if (!("ForcedValidationSource" in membership))
            throw new Error("expected forced source");
          const resolveFieldCarriage = await input.prepareFieldCarriage!({
            trace: fixture.challengerTrace,
            stateIndex: fixture.disputedLowIndex,
            source:
              membership.ForcedValidationSource.membership.value
                .submitted_source,
          });
          return {
            ...fixture,
            disputedPhase: "canonicalDecode" as const,
            claimedLedgerDeltaRoot:
              fixture.operatorTrace.states[0]!.ledgerDeltaRoot,
            evidence: buildValidationDisputeEvidenceBundle({
              operatorTrace: fixture.operatorTrace,
              challengerTrace: fixture.challengerTrace,
              currentTime: input.now + 2000,
              resolveFieldCarriage,
            }),
          };
        },
        { canonicalItemMaximum: true, directCommittedStep: true },
      ),
    {
      refusedBy:
        "fraud_proofs/validation_trace/canonical_decode_item_settlement_v1",
      check: /Validator returned false/u,
    },
  );
  expect(failure).toContain("semantic-resolution");
}, 600_000);
