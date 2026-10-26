/**
 * The direct purpose source walk over a deep predecessor ledger, from a
 * genuine Init: sixteen spend inputs and eight reference inputs, each output
 * carrying its own reference script, are ground into one ledger-trie path
 * cluster behind a full branch at each shared digit, so every input's
 * membership opens at least five branches deep. Each source page is sized
 * to one transaction's budget by the shared page fit, and a missing redeemer
 * at a reference-sourced observer convicts.
 */
import { hashMidgardVersionedScript } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { keyValuePhasMembershipProofs } from "../src/transition-trace/phas.js";
import { clusterPadding, intoCluster } from "./support/deep-ledger-trie.js";
import {
  driveMissingRedeemerThread,
  makeMissingRedeemerHarness,
  MISSING_REDEEMER_STEP_BOUNDS,
  publishMissingRedeemerRoles,
  type StepMeasurement,
  stepTable,
} from "./support/missing-redeemer-emulator.js";
import {
  missingRedeemerLedgerEntries,
  type MissingRedeemerSubjectShape,
  plutusScript,
  setupMissingRedeemerScenario,
} from "./support/missing-redeemer-emulator.subject.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";

const SPENDS = 16;
const REFERENCES = 8;
/** The reference inputs' path cluster; the spend inputs share its first
 * five digits. */
const REFERENCE_CLUSTER = "a5c3e9b7";
const SPEND_CLUSTER = REFERENCE_CLUSTER.slice(0, 5);

/** Each ground out-ref's first accepted nonce, recorded so the searches
 * start where they end (each is re-checked against its cluster). */
const FIRST_NONCES = {
  spend: [
    80679n,
    449231n,
    426674n,
    2156817n,
    4192642n,
    1144979n,
    222137n,
    421296n,
    1357776n,
    1725562n,
    502928n,
    243121n,
    555734n,
    343954n,
    421234n,
    489208n,
  ],
  reference: [
    3707647191n,
    4034171828n,
    12422552239n,
    1577775210n,
    5695688245n,
    8777524122n,
    681379952n,
    1889196097n,
  ],
  padding: [
    [37n, 58n, 14n, 13n, 1n, 1n, 16n, 14n, 24n, 20n, 84n, 26n, 9n, 1n, 46n],
    [
      338n,
      241n,
      146n,
      738n,
      409n,
      15n,
      282n,
      36n,
      50n,
      421n,
      656n,
      7n,
      410n,
      274n,
      34n,
    ],
    [
      7257n,
      4353n,
      1124n,
      686n,
      4031n,
      3183n,
      22193n,
      12876n,
      3548n,
      4515n,
      685n,
      5240n,
      15279n,
      6412n,
      6552n,
    ],
    [
      6222n,
      45986n,
      21945n,
      287199n,
      1634n,
      18426n,
      67769n,
      39518n,
      226882n,
      42432n,
      12286n,
      37441n,
      207722n,
      42054n,
      149866n,
    ],
    [
      342197n,
      2315350n,
      467883n,
      2149793n,
      23372n,
      510121n,
      625989n,
      1073605n,
      1266971n,
      1721988n,
      213599n,
      118950n,
      654347n,
      95861n,
      178498n,
    ],
    [
      12257465n,
      43455662n,
      9906622n,
      13451730n,
      22878142n,
      23513872n,
      10116551n,
      9055863n,
      24801526n,
      42796369n,
      23132861n,
      15565669n,
      26227855n,
      10555011n,
      13158450n,
    ],
    [
      1657357456n,
      463873695n,
      320120793n,
      67936308n,
      251964577n,
      14406750n,
      36209683n,
      91906087n,
      381624244n,
      970885261n,
      309455690n,
      35213651n,
      47037005n,
      450083137n,
      225808194n,
    ],
    [
      2188913258n,
      939845396n,
      11386368700n,
      1806025541n,
      2958304247n,
      2047004260n,
      2647107311n,
      3705177192n,
      1848344279n,
      14199330n,
      8199965037n,
      516480476n,
      2788212509n,
      2228844141n,
      180077104n,
    ],
  ],
};

/** The observer's Plutus script, sourced only from the first reference
 * input. */
const OBSERVED = plutusScript(1);

const deepSourceShape = (): MissingRedeemerSubjectShape => ({
  observers: [{ script: OBSERVED, witness: false, redeemer: false }],
  spend: intoCluster(
    missingRedeemerLedgerEntries(
      0x41,
      Array.from({ length: SPENDS }, (_, index) => plutusScript(100 + index)),
    ),
    0x41,
    SPEND_CLUSTER,
    FIRST_NONCES.spend,
  ),
  reference: intoCluster(
    missingRedeemerLedgerEntries(0x31, [
      OBSERVED,
      ...Array.from({ length: REFERENCES - 1 }, (_, index) =>
        plutusScript(200 + index),
      ),
    ]),
    0x31,
    REFERENCE_CLUSTER,
    FIRST_NONCES.reference,
  ),
  padding: clusterPadding(REFERENCE_CLUSTER, FIRST_NONCES.padding),
});

/** Each input page's item count in field `field`, from the start offsets of
 * its resolve transactions. */
const pageSizes = (
  steps: readonly StepMeasurement[],
  field: number,
  count: number,
): number[] => {
  const prefix = `field ${field.toString()} resolve `;
  const starts = steps.flatMap(({ label }) =>
    label.startsWith(prefix) ? [Number(label.slice(prefix.length))] : [],
  );
  return starts.map((start, index) => (starts[index + 1] ?? count) - start);
};

const expectWithinBounds = (steps: readonly StepMeasurement[]): void => {
  for (const { label, measurement } of steps) {
    expect(measurement.executionMemory, `${label} memory`).toBeLessThanOrEqual(
      MISSING_REDEEMER_STEP_BOUNDS.memory,
    );
    expect(measurement.executionSteps, `${label} cpu`).toBeLessThanOrEqual(
      MISSING_REDEEMER_STEP_BOUNDS.steps,
    );
    expect(
      measurement.completeSignedBytes,
      `${label} bytes`,
    ).toBeLessThanOrEqual(MISSING_REDEEMER_STEP_BOUNDS.bytes);
  }
};

const peak = (steps: readonly StepMeasurement[]) => {
  const table = stepTable(steps);
  return table.reduce(
    (best, row) => (BigInt(row.memory) > BigInt(best.memory) ? row : best),
    table[0]!,
  );
};

describe("direct purpose source walk over a deep ledger", () => {
  it("fits each source page to the budget and convicts a missing observer redeemer", async () => {
    const harness = await makeMissingRedeemerHarness();
    const references = await publishMissingRedeemerRoles(harness);
    const shape = deepSourceShape();
    const scenario = await setupMissingRedeemerScenario({
      harness,
      shape,
      source: { kind: "normal" },
    });
    const { ledger } = scenario.material;
    const ledgerDepths = (
      await keyValuePhasMembershipProofs(
        ledger.root,
        [...scenario.material.fields[0]!, ...scenario.material.fields[1]!].map(
          (item) =>
            ledger.root.entries.find((entry) => entry.key.equals(item))!,
        ),
      )
    ).map((proof) => proof.length);
    expect(ledgerDepths).toHaveLength(SPENDS + REFERENCES);
    expect(Math.min(...ledgerDepths)).toBeGreaterThanOrEqual(5);

    const run = await driveMissingRedeemerThread({
      harness,
      scenario,
      references,
    });
    expect(run.prepared.convicts).toBe(true);
    const sizes = {
      spend: pageSizes(run.steps, 0, SPENDS),
      reference: pageSizes(run.steps, 1, REFERENCES),
    };
    const source = run.steps.filter(({ label }) => /^field [01] /u.test(label));
    console.log(
      "direct purpose deep source walk",
      JSON.stringify({
        ledgerDepth: [Math.min(...ledgerDepths), Math.max(...ledgerDepths)],
        pageSizes: sizes,
        sourcePeak: peak(source),
        peak: peak(run.steps),
        transactions: run.steps.length,
      }),
    );
    expectWithinBounds(run.steps);
    expect(sizes.spend.reduce((sum, size) => sum + size, 0)).toBe(SPENDS);
    expect(sizes.reference.reduce((sum, size) => sum + size, 0)).toBe(
      REFERENCES,
    );
    const outcome = await run.terminal();
    expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
    await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.missingRedeemer.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
    // The observer's only source is the reference input it was ground with.
    expect(
      (await run.prepared.evidence.lookup(hashMidgardVersionedScript(OBSERVED)))
        .selected,
    ).not.toBeNull();
  }, 3_600_000);
});
