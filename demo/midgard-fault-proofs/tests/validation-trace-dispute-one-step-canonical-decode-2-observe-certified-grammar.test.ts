/** The validation-trace dispute runner at the canonical-decode step of the
 * last item of the failing transaction's outputs field, a field of more
 * items than the observe stage walks within a step's budget. While the
 * successor's thread is in its game, the runner builds the field's grammar
 * certificate; the observe move then reads the item through it
 * (CertifiedFieldItem), so its cost no longer grows with the field's item
 * count, and the thread plays through to the award
 * (validation-trace-dispute-one-step-canonical-decode-8-observe-certified.test.ts
 * covers a field past one publication, read by the walking route). */
import { hashMidgardValidationMachineState } from "@al-ft/midgard-core";
import { CML, Constr, Data } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import {
  findValidationTraceFieldGrammarCertificate,
  VALIDATION_TRACE_WALK_ROUTE_MAX_ITEMS,
  validationTraceFieldItemRead,
  validationTraceFieldItemRoute,
} from "../src/validation-trace-dispute/one-step-field-item-route.js";
import { planValidationTraceOneStep } from "../src/validation-trace-dispute/one-step-plan.js";
import {
  landed,
  outRefsOf,
} from "./support/validation-trace-dispute-observe-copies.js";
import { canonicalDecodeDrive } from "./support/validation-trace-dispute-one-step-canonical-decode-drive.js";
import { reportFit } from "./support/validation-trace-dispute-scripted-defender.js";

/** The outputs the failing transaction pays past its own: its outputs
 * field holds one more item than this, past the walking route's bound and
 * past the most items the observe stage's walk reads within a step's
 * budget (about 180). */
const EXTRA_OUTPUTS = 199;
const ITEMS = EXTRA_OUTPUTS + 1;
const OUTPUTS_FIELD = 2;

/** The canonical-decode steps go one an item, and the outputs field's
 * first item is the step after state 2: its last item's is after this. */
const FROM = 2 + EXTRA_OUTPUTS;

const OBSERVE = "Observe canonicalDecode item";

/** The data of each redeemer `transaction` carries. */
const redeemerData = (transaction: CML.Transaction): unknown[] => {
  const redeemers = transaction.witness_set().redeemers();
  const found: unknown[] = [];
  const legacy = redeemers?.as_arr_legacy_redeemer();
  for (let index = 0; legacy !== undefined && index < legacy.len(); index += 1)
    found.push(Data.from(legacy.get(index).data().to_cbor_hex()));
  const map = redeemers?.as_map_redeemer_key_to_redeemer_val();
  const keys = map?.keys();
  for (let index = 0; keys !== undefined && index < keys.len(); index += 1)
    found.push(Data.from(map!.get(keys.get(index))!.data().to_cbor_hex()));
  return found;
};

describe("validation-trace dispute runner: the canonical-decode step of the last of 200 outputs, read through the field's grammar certificate", () => {
  const drive = canonicalDecodeDrive({
    from: FROM,
    kind: "item",
    at: "canonicalDecodeItemObserve",
    failingOutputs: EXTRA_OUTPUTS,
  });

  /** The `Complete` certificate of the outputs field, under our key. */
  const certificate = async () => {
    const { scene, deployment, material } = drive.dispute();
    const found = await findValidationTraceFieldGrammarCertificate({
      lucid: scene.h.proverLucid,
      deployment,
      material,
      fieldIndex: OUTPUTS_FIELD,
      owner: scene.h.proverSigner.paymentKeyHash,
    });
    if (found === undefined)
      throw new Error("the outputs field has no Complete certificate");
    return found;
  };

  beforeAll(drive.setUp, 1_800_000);

  it(
    "routes the step to the item semantic resolver, committing the transition alone",
    drive.routes,
    300_000,
  );

  it("plans the outputs field's last item, inline, over a field the route reads through its certificate", async () => {
    const { material } = drive.dispute();
    const plan = await drive.plan();
    const item = plan.canonicalDecode.item!;
    expect(item.fieldIndex).toBe(OUTPUTS_FIELD);
    expect(item.carried).toBeUndefined();
    expect((Data.from(item.observationCbor) as Constr<bigint>).fields[0]).toBe(
      BigInt(ITEMS),
    );
    expect(ITEMS).toBeGreaterThan(VALIDATION_TRACE_WALK_ROUTE_MAX_ITEMS);
    expect(
      validationTraceFieldItemRoute(
        validationTraceFieldItemRead(plan)!,
        material.fieldPreimage(OUTPUTS_FIELD),
      ),
    ).toBe("certified");
    // The step after it reads no item of the outputs field: the item is the
    // field's last.
    const next = await planValidationTraceOneStep({
      material,
      stateIndex: FROM + 1,
      challengerHighHash: hashMidgardValidationMachineState(
        (await material.steps(FROM + 1)).states[FROM + 2]!,
      ).toString("hex"),
      canonicalDecode: true,
    });
    expect(
      next.kind === "plan" &&
        next.plan.kind === "canonicalDecode" &&
        next.plan.canonicalDecode.item?.fieldIndex === OUTPUTS_FIELD,
    ).toBe(false);
  }, 600_000);

  it(
    "moves the thread through the step's resolution to the award, and closes it",
    drive.awards,
    1_200_000,
  );

  it("built the field's grammar certificate while the thread was in its game, before it entered resolution", async () => {
    const { scene, deployment } = drive.dispute();
    const policy =
      deployment.fieldGrammarCertificate!.validator.spendingScriptHash;
    const txs = landed(scene);
    const certifying = txs.flatMap(({ tx }, at) => {
      const outputs = tx.body().outputs();
      for (let index = 0; index < outputs.len(); index += 1)
        if (
          outputs.get(index).address().payment_cred()?.as_script()?.to_hex() ===
          policy
        )
          return [at];
      return [];
    });
    expect(certifying.length).toBeGreaterThan(0);
    const entered = txs.findIndex(
      ({ cbor }) => cbor === drive.cborOf("EnterResolution"),
    );
    expect(entered).toBeGreaterThan(-1);
    expect(Math.max(...certifying)).toBeLessThan(entered);
    const complete = await certificate();
    expect(certifying.some((at) => complete.txHash === txs[at]!.hash)).toBe(
      true,
    );
  });

  it("observes the item through the certificate: a reference input its route names by index, beside the evidence", async () => {
    const complete = await certificate();
    const outRef = `${complete.txHash}#${complete.outputIndex.toString()}`;
    const transaction = CML.Transaction.from_cbor_hex(drive.cborOf(OBSERVE));
    // The ledger orders reference inputs by transaction id, then index.
    const index = outRefsOf(transaction.body().reference_inputs())
      .map((label) => label.split("#") as [string, string])
      .sort(([a, i], [b, j]) =>
        a === b ? Number(i) - Number(j) : a < b ? -1 : 1,
      )
      .map(([hash, at]) => `${hash}#${at}`)
      .indexOf(outRef);
    expect(index).toBeGreaterThan(-1);
    // The thread's redeemer continues the game with the observe action,
    // whose last field is its route.
    const routes = redeemerData(transaction).flatMap((data) => {
      const action =
        data instanceof Constr && data.index === 1 ? data.fields[0] : undefined;
      return action instanceof Constr && action.fields.length > 0
        ? [Data.to(action.fields.at(-1) as never)]
        : [];
    });
    expect(routes).toContain(Data.to(new Constr(1, [BigInt(index)])));
    reportFit(
      `${OBSERVE} (CertifiedFieldItem, ${ITEMS.toString()}-output field)`,
      [drive.cborOf(OBSERVE)],
    );
  });

  it(
    "pays each move from its thread when its fee fits under the cap, and from our wallet when it does not",
    drive.pays,
  );
});
