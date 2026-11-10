/**
 * `midgard-user forced-order` and `plutus-tx` against a deployed emulator
 * scene (the genuine tx-order mint, evaluated locally) and an in-process L2
 * node that judges submissions with the node's own validation.
 *
 * forced-order: every admitted shape mints an order whose datum carries the
 * printed L2 transaction id, with the shape's own field present in the forced
 * bytes; every refused shape is stopped at the screen check the mint applies,
 * naming it, and nothing reaches L1. plutus-tx: the PlutusV3 mint passes the
 * node's Phase A and B, and a node verdict of rejection fails the command.
 */
import {
  decodeMidgardForcedTxFullFromCanonicalCbor,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { decodeTxOrderDatumCbor } from "@al-ft/midgard-sdk";
import { Constr, Data, getAddressDetails } from "@lucid-evolution/lucid";
import { beforeAll, describe, expect, it } from "vitest";

import { inProcessL2Node, l2Output } from "./support/l2-node.js";
import {
  printedJson,
  SCENE_TIMEOUT_MS,
  setupUserCliHarness,
  type UserCliHarness,
} from "./support/user-cli.js";

const TEST_TIMEOUT_MS = 180_000;

/** Independent of the command's own constants: what each shape must carry. */
const ASSET_NAME_HEX = Buffer.from("midgard-rehearsal").toString("hex");
const INLINE_DATUM_HEX = Data.to(new Constr(0, [ASSET_NAME_HEX, 1n]));

const hex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");

let harness: UserCliHarness;
let ownKeyHash: string;
let outputs: ReturnType<typeof l2Output>[];

const node = (ledger = outputs) =>
  inProcessL2Node({
    manifestId: harness.manifest.manifestId,
    outputs,
    ledger,
    l1SlotConfig: harness.slotConfig,
  });

type PrintedForcedOrder = {
  readonly shape: string;
  readonly l2TxId: string;
  readonly forcedTxCbor: string;
  readonly orderTxHash: string;
  readonly txOrderAddress: string;
};

const forcedOrder = (shape: string) =>
  harness.run(
    [
      "forced-order",
      "--shape",
      shape,
      "--node-endpoint",
      "http://127.0.0.1:9/unused",
      "--json",
    ],
    { overrides: { connectL2: async () => node().provider } },
  );

const walletL1OutRefs = async () =>
  (await harness.scene.lucid.utxosAt(harness.scene.user.address))
    .map((utxo) => `${utxo.txHash}#${utxo.outputIndex.toString()}`)
    .sort();

beforeAll(async () => {
  harness = await setupUserCliHarness();
  const { address } = harness.scene.user;
  ownKeyHash = getAddressDetails(address).paymentCredential!.hash;
  outputs = [l2Output(address, 0xa1), l2Output(address, 0xa2)];
}, SCENE_TIMEOUT_MS);

describe("midgard-user forced-order", () => {
  it.each([
    {
      shape: "plain",
      carries: (body: ForcedBody) =>
        !hex(body.outputsPreimageCbor).includes(INLINE_DATUM_HEX) &&
        !hex(body.mintPreimageCbor).includes(ASSET_NAME_HEX) &&
        !hex(body.referenceInputsPreimageCbor).includes(outputs[1]!.txHash) &&
        !hex(body.requiredSignersPreimageCbor).includes(ownKeyHash),
    },
    {
      shape: "inline-datum",
      carries: (body: ForcedBody) =>
        hex(body.outputsPreimageCbor).includes(INLINE_DATUM_HEX),
    },
    {
      shape: "native-mint",
      carries: (body: ForcedBody) =>
        hex(body.mintPreimageCbor).includes(ASSET_NAME_HEX),
    },
    {
      shape: "reference-input",
      carries: (body: ForcedBody) =>
        hex(body.referenceInputsPreimageCbor).includes(outputs[1]!.txHash),
    },
    {
      shape: "required-signer",
      carries: (body: ForcedBody) =>
        hex(body.requiredSignersPreimageCbor).includes(ownKeyHash),
    },
  ])(
    "mints an order carrying the $shape transaction",
    async ({ shape, carries }) => {
      const result = await forcedOrder(shape);
      expect(result.stderr).not.toContain("midgard-user:");
      expect(result.code).toBe(0);
      const printed = printedJson(result.stdout) as PrintedForcedOrder;
      expect(printed.shape).toBe(shape);
      expect(
        carries(
          decodeMidgardForcedTxFullFromCanonicalCbor(
            Buffer.from(printed.forcedTxCbor, "hex"),
          ).body,
        ),
      ).toBe(true);

      const order = (
        await harness.scene.lucid.utxosAt(printed.txOrderAddress)
      ).find((utxo) => utxo.txHash === printed.orderTxHash);
      expect(order?.datum).toBeTypeOf("string");
      expect(
        decodeTxOrderDatumCbor(Buffer.from(order!.datum!, "hex")).event.tx
          .tx_id,
      ).toBe(printed.l2TxId);
    },
    TEST_TIMEOUT_MS,
  );

  it.each([
    {
      shape: "validity-upper-bound",
      check: "has_no_validity_upper_bound(body)",
    },
    { shape: "plutus-mint", check: "has_no_redeemers(witness_set)" },
    {
      shape: "reference-script-output",
      check: "outputs_are_admissible(outputs)",
    },
  ])(
    "refuses the $shape transaction at $check and submits nothing",
    async ({ shape, check }) => {
      const before = await walletL1OutRefs();
      const result = await forcedOrder(shape);
      expect(result.code).toBe(1);
      expect(result.stderr).toContain(`at ${check}:`);
      expect(result.stderr).toContain("Nothing was submitted.");
      expect(await walletL1OutRefs()).toEqual(before);
    },
    TEST_TIMEOUT_MS,
  );
});

type ForcedBody = ReturnType<
  typeof decodeMidgardForcedTxFullFromCanonicalCbor
>["body"];

describe("midgard-user plutus-tx", () => {
  const plutusTx = (l2: ReturnType<typeof node>) =>
    harness.run(
      ["plutus-tx", "--node-endpoint", "http://127.0.0.1:9/unused", "--json"],
      { access: [], overrides: { connectL2: async () => l2.provider } },
    );

  it(
    "submits a PlutusV3 mint the node's Phase A and B accept",
    async () => {
      const l2 = node();
      const result = await plutusTx(l2);
      expect(result.stderr).not.toContain("midgard-user:");
      expect(result.code).toBe(0);
      const printed = printedJson(result.stdout) as {
        readonly l2TxId: string;
        readonly status: string;
        readonly policyId: string;
      };
      expect(printed.status).toBe("accepted");
      expect(l2.submitted()).toHaveLength(1);
      const mint = hex(
        decodeMidgardNativeTxFullFromCanonicalCbor(
          Buffer.from(l2.submitted()[0]!, "hex"),
        ).body.mintPreimageCbor,
      );
      expect(mint).toContain(printed.policyId);
      expect(mint).toContain(ASSET_NAME_HEX);
    },
    TEST_TIMEOUT_MS,
  );

  it(
    "fails with the node's rejection when its input is spent before submission",
    async () => {
      const result = await plutusTx(node([]));
      expect(result.code).toBe(1);
      expect(result.stderr).toMatch(
        /midgard-user: The node rejected L2 transaction [0-9a-f]{64}: /u,
      );
    },
    TEST_TIMEOUT_MS,
  );
});
