import {
  computeHash28,
  encodeMidgardFieldPreimage,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScript,
  MIDGARD_FIELD_COUNT,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { prepareDirectScriptMaterial } from "../../src/direct-script-derivation/index.js";
import { reconstructDaPayload } from "../../src/transition-trace/reconstruct.js";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
} from "../helpers/canonical-block-evidence-fixture.js";

/** Sixty-four distinct credentials in deliberately unsorted output order, two
 * repetitions, and a plain output. Pad only canonical datum bytes, distributing
 * them so the output field exactly consumes the aggregate allowance remaining
 * after the other eight empty field envelopes. */
export const maximumDirectReceiveOutputs = (): readonly Buffer[] => {
  const credentials = Array.from({ length: 64 }, (_, index) =>
    computeHash28(
      Buffer.from(`N6 receive credential ${index}`, "ascii"),
    ).toString("hex"),
  );
  const hashes = [
    ...credentials.reverse(),
    credentials[17]!,
    credentials[0]!,
    credentials[9]!,
  ];
  const makeOutput = (index: number, padding: number) =>
    encodeMidgardTxOutput({
      address: Buffer.from(
        (index === hashes.length - 1 ? "70" : "78") + hashes[index]!,
        "hex",
      ),
      value: { lovelace: 5_000_000n, assets: new Map() },
      datum: {
        kind: "inline",
        cbor: Buffer.from(Data.to("ab".repeat(padding)), "hex"),
      },
    });
  const target =
    MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES - (MIDGARD_FIELD_COUNT - 1);
  const outputs = hashes.map((_, index) => makeOutput(index, 400));
  for (let padding = 0; padding < 4096; padding += 1) {
    outputs[outputs.length - 1] = makeOutput(outputs.length - 1, padding);
    const bytes = encodeMidgardFieldPreimage(outputs).length;
    if (bytes === target) return outputs;
    if (bytes > target) break;
  }
  throw new Error(
    "N6 maximum receive fixture cannot fill the exact aggregate allowance",
  );
};

/** The receive lifecycle's committed transaction: the given outputs, or two
 * protected credentials (one repeated) plus a plain output by default. */
export const directReceiveLifecycleMaterial = async (
  given?: readonly Buffer[],
) => {
  const a = "22".repeat(28),
    b = "33".repeat(28);
  const output = (hash: string, protectedAddress = true) =>
    encodeMidgardTxOutput({
      address: Buffer.from((protectedAddress ? "78" : "70") + hash, "hex"),
      value: { lovelace: 5_000_000n, assets: new Map() },
    });
  const outputs = given ?? [output(b), output(a), output(b), output(a, false)];
  // The ordinary bridge carries a genuine nonempty source map. The maximum
  // output fixture already consumes the full aggregate field-byte allowance
  // with the other eight fields empty, so it retains that admitted shape.
  const scriptWitnesses =
    given === undefined
      ? [
          encodeMidgardVersionedScript({
            language: "PlutusV3",
            scriptBytes: Buffer.from([1, 2]),
          }),
        ]
      : [];
  const tx = buildFixtureTransaction({
    spendInputs: [],
    outputs,
    scriptWitnesses,
    fee: 0n,
  });
  const eventKey: SDK.EventKey = { L2TransactionEventKey: { tx_id: tx.txId } };
  const fixture = await buildCanonicalBlockFixture({
    transactions: [tx],
    transitionSteps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: eventKey,
        phase: "L2Transaction",
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
    ],
  });
  const material = await prepareDirectScriptMaterial({
    current: await reconstructDaPayload({
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    }),
    eventKey,
  });
  return { outputs, scriptWitnesses, material };
};
