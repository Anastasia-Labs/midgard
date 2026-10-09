/**
 * missingScriptSource covers a spent native-script output wherever its
 * script is committed: the family's universal-source scan walks the
 * transaction's inline script witnesses (field 6) and then the scripts its
 * reference inputs resolve to. These regressions pin both polarities over a
 * native-script-locked spend:
 *
 * - an honest accepted block whose native script arrives only through a
 *   resolved reference input, with field 6 empty, is refused;
 * - the same honest block with the native script inline is refused;
 * - a block that commits the spend with no source anywhere is convicted,
 *   ending in the permanent fraud-proof token.
 */
import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { missingScriptSourceEvidenceCloses } from "../src/missing-script-source/family.js";
import { discoverRetainedMissingScriptSourceCoordinates } from "../src/missing-script-source/retained-script-universe.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import {
  buildMissingScriptSourceFixture,
  buildMissingScriptSourceUniverse,
  commitMissingScriptSourceBlock,
  makeMissingScriptSourceHarness,
  makeMissingScriptSourceStages,
  missingScriptSourceEvidence,
  runMissingScriptSourceThread,
} from "./support/missing-script-source-emulator.js";

const inlineWitnessCount = (
  fixture: Awaited<ReturnType<typeof buildMissingScriptSourceFixture>>,
) =>
  decodeMidgardFieldPreimage(
    fixture.transaction.tx.witnessSet.scriptTxWitsPreimageCbor,
  ).length;

describe("missingScriptSource native-script source provenance", () => {
  it.each(["reference", "inline"] as const)(
    "refuses an honest native-script spend whose script is committed %s",
    async (presentAt) => {
      const fixture = await buildMissingScriptSourceFixture({
        purposeKind: 0,
        presentAt,
        inlineDecoys: 0,
        referenceDecoys: 0,
        direction: "accepted",
        honest: true,
      });
      // The reference variant commits no inline witness at all: the native
      // script reaches the transaction only through its resolved reference
      // input. The inline variant commits it in field 6 and no reference.
      expect(inlineWitnessCount(fixture)).toBe(
        presentAt === "reference" ? 0 : 1,
      );
      expect(fixture.sourceCount).toBe(1);
      expect(fixture.transactionSourceCount).toBe(
        presentAt === "reference" ? 0 : 1,
      );
      // Off chain: the machine accepted the spend, so no retained absence
      // coordinate exists and a no-match universe cannot be built.
      expect(
        discoverRetainedMissingScriptSourceCoordinates({
          eventKey: fixture.eventKey,
          retainedValidationWitnessEntries: fixture.retainedEntries,
        }),
      ).toEqual([]);
      await expect(
        buildMissingScriptSourceUniverse(fixture, false),
      ).rejects.toThrow(/absent or duplicated/u);
      const context = await makeMissingScriptSourceHarness();
      const block = await commitMissingScriptSourceBlock({
        harness: context.harness,
        catalogue: context.catalogue,
        fixture,
      });
      const matched = await buildMissingScriptSourceUniverse(fixture, true);
      const evidence = missingScriptSourceEvidence({
        fixture,
        universe: matched,
        nativeTxId: block.nativeTxId,
      });
      expect(evidence.foundAtSourceIndex).toBe(fixture.presentSourceIndex);
      expect(missingScriptSourceEvidenceCloses(evidence)).toBe(false);
      // On chain: the bind and trace authentication admit the honest block,
      // and step 03 refuses the wrongful-acceptance claim.
      const stages = makeMissingScriptSourceStages(context, block);
      const bound = await stages.step02(
        await stages.step01(await stages.init(), evidence),
        evidence,
        matched.authentication,
      );
      await expectOnchainRefusal(() =>
        stages.step03(bound, evidence, matched.authentication),
      );
      await stages.cancel(bound, 2);
    },
    300_000,
  );

  it("convicts a native-script spend committed with no source anywhere", async () => {
    const fixture = await buildMissingScriptSourceFixture({
      purposeKind: 0,
      presentAt: "absent",
      inlineDecoys: 1,
      referenceDecoys: 1,
      direction: "accepted",
    });
    const context = await makeMissingScriptSourceHarness();
    const block = await commitMissingScriptSourceBlock({
      harness: context.harness,
      catalogue: context.catalogue,
      fixture,
    });
    const universe = await buildMissingScriptSourceUniverse(fixture);
    const evidence = missingScriptSourceEvidence({
      fixture,
      universe,
      nativeTxId: block.nativeTxId,
    });
    expect(missingScriptSourceEvidenceCloses(evidence)).toBe(true);
    const stages = makeMissingScriptSourceStages(context, block);
    const { final } = await runMissingScriptSourceThread(
      stages,
      evidence,
      universe.authentication,
    );
    const [permanent] = await context.harness.proverLucid.utxosAtWithUnit(
      context.harness.contracts.fraudProof.spendingScriptAddress,
      final.fraudProofUnit,
    );
    expect(permanent?.txHash).toBe(final.txHash);
  }, 300_000);
});
