/**
 * The L2 transactions `forced-order` and `plutus-tx` build with lucid-midgard
 * from the wallet's own L2 outputs.
 *
 * Forced shapes follow `screenMidgardForcedTxAdmission`, the off-chain twin of
 * the tx-order mint's `forced_tx_is_admissible_v1`. Each admitted shape carries
 * one field the screen reads and admits; each refused shape breaks one check
 * a builder-made transaction can break. The remaining checks (no spend input,
 * auxiliary data, observers on an untagged network, an output value over
 * `maxOutputValueCborBytes`, and the fixed item prefixes and mint order) are
 * reachable only with bytes lucid-midgard never builds: its consensus check
 * or its codec refuses them first.
 */
import {
  type LucidMidgard,
  type MidgardUtxo,
  type TxBuilder,
} from "@al-ft/lucid-midgard";
import {
  decodeMidgardNativeScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core/codec";
import { type MidgardForcedTxAdmissionCheck } from "@al-ft/midgard-core/consensus-validation";
import { CML, Constr, Data, fromText } from "@lucid-evolution/lucid";

/** `(program 1.1.0 (lam context (con unit ())))` as stored: one CBOR byte
 * string wrapping the Flat program. It accepts any script context. */
export const ALWAYS_SUCCEEDS_PLUTUS_V3 = Buffer.from("450101002499", "hex");

export const ALWAYS_SUCCEEDS_PLUTUS_V3_HASH = hashMidgardVersionedScript({
  language: "PlutusV3",
  scriptBytes: ALWAYS_SUCCEEDS_PLUTUS_V3,
});

/** The Plutus mint's redeemer: `Constr 0 []`. */
const UNIT_REDEEMER = Data.to(new Constr(0, []));

const ASSET_NAME = fromText("midgard-rehearsal");

const PAYMENT_LOVELACE = 2_000_000n;

export type ShapeWallet = {
  readonly address: string;
  readonly paymentKeyHash: string;
  /** The wallet's L2 outputs, in the node's order. */
  readonly utxos: readonly MidgardUtxo[];
  /** The node's current slot. */
  readonly currentSlot: bigint;
};

type Shape = {
  readonly summary: string;
  /** How many of the wallet's L2 outputs the shape uses. */
  readonly inputs: number;
  /** The screen's check this shape fails, or null when it is admitted. */
  readonly refusedAt: MidgardForcedTxAdmissionCheck | null;
  readonly build: (midgard: LucidMidgard, wallet: ShapeWallet) => TxBuilder;
};

/** A native policy only the wallet's key can mint under. */
const walletNativePolicy = (paymentKeyHash: string) => {
  const script = CML.NativeScript.new_script_pubkey(
    CML.Ed25519KeyHash.from_hex(paymentKeyHash),
  );
  const decoded = decodeMidgardNativeScript(script.to_cbor_bytes());
  return {
    script,
    policyId: hashMidgardVersionedScript({
      language: "NativeCardano",
      scriptBytes: decoded.cbor,
      nativeScript: decoded.script,
    }),
  };
};

/** The wallet's first output, paying part of it back to the wallet. */
const spendFirst = (midgard: LucidMidgard, wallet: ShapeWallet) =>
  midgard.newTx().collectFrom([wallet.utxos[0]!]);

const plutusMint = (midgard: LucidMidgard, wallet: ShapeWallet) =>
  spendFirst(midgard, wallet)
    .attach.Script({
      kind: "plutus-v3",
      language: "PlutusV3",
      script: ALWAYS_SUCCEEDS_PLUTUS_V3,
    })
    .mintAssets(
      ALWAYS_SUCCEEDS_PLUTUS_V3_HASH,
      { [ASSET_NAME]: 1n },
      { data: UNIT_REDEEMER },
    )
    .pay.ToAddress(wallet.address, {
      lovelace: PAYMENT_LOVELACE,
      [`${ALWAYS_SUCCEEDS_PLUTUS_V3_HASH}${ASSET_NAME}`]: 1n,
    });

export const FORCED_ORDER_SHAPES = {
  plain: {
    summary: "one spend input and outputs without data",
    inputs: 1,
    refusedAt: null,
    build: (midgard, wallet) =>
      spendFirst(midgard, wallet).pay.ToAddress(wallet.address, {
        lovelace: PAYMENT_LOVELACE,
      }),
  },
  "inline-datum": {
    summary: "an output carrying an inline datum",
    inputs: 1,
    refusedAt: null,
    build: (midgard, wallet) =>
      spendFirst(midgard, wallet).pay.ToAddress(
        wallet.address,
        { lovelace: PAYMENT_LOVELACE },
        {
          datum: {
            kind: "inline",
            data: Data.to(new Constr(0, [ASSET_NAME, 1n])),
          },
        },
      ),
  },
  "native-mint": {
    summary: "a native-script witness minting under the wallet's own policy",
    inputs: 1,
    refusedAt: null,
    build: (midgard, wallet) => {
      const policy = walletNativePolicy(wallet.paymentKeyHash);
      return spendFirst(midgard, wallet)
        .attach.NativeScript(policy.script)
        .mintAssets(policy.policyId, { [ASSET_NAME]: 1n })
        .pay.ToAddress(wallet.address, {
          lovelace: PAYMENT_LOVELACE,
          [`${policy.policyId}${ASSET_NAME}`]: 1n,
        });
    },
  },
  "reference-input": {
    summary: "a reference input beside the spend input",
    inputs: 2,
    refusedAt: null,
    build: (midgard, wallet) =>
      spendFirst(midgard, wallet)
        .readFrom([wallet.utxos[1]!])
        .pay.ToAddress(wallet.address, { lovelace: PAYMENT_LOVELACE }),
  },
  "required-signer": {
    summary: "the wallet's key as a required signer",
    inputs: 1,
    refusedAt: null,
    build: (midgard, wallet) =>
      spendFirst(midgard, wallet)
        .addSignerKey(wallet.paymentKeyHash)
        .pay.ToAddress(wallet.address, { lovelace: PAYMENT_LOVELACE }),
  },
  "validity-upper-bound": {
    summary: "an L2 validity upper bound",
    inputs: 1,
    refusedAt: "has_no_validity_upper_bound(body)",
    build: (midgard, wallet) =>
      spendFirst(midgard, wallet)
        .validTo(wallet.currentSlot + 3_600n)
        .pay.ToAddress(wallet.address, { lovelace: PAYMENT_LOVELACE }),
  },
  "plutus-mint": {
    summary: "a PlutusV3 mint, which carries a redeemer",
    inputs: 1,
    refusedAt: "has_no_redeemers(witness_set)",
    build: plutusMint,
  },
  "reference-script-output": {
    summary: "an output carrying a reference script",
    inputs: 1,
    refusedAt: "outputs_are_admissible(outputs)",
    build: (midgard, wallet) =>
      spendFirst(midgard, wallet).pay.ToAddress(
        wallet.address,
        { lovelace: PAYMENT_LOVELACE },
        {
          scriptRef: {
            type: "PlutusV3",
            script: ALWAYS_SUCCEEDS_PLUTUS_V3.toString("hex"),
          },
        },
      ),
  },
} as const satisfies Record<string, Shape>;

export type ForcedOrderShape = keyof typeof FORCED_ORDER_SHAPES;

export const FORCED_ORDER_SHAPE_NAMES = Object.keys(
  FORCED_ORDER_SHAPES,
) as ForcedOrderShape[];

/** The normal L2 Plutus transaction: a PlutusV3 mint the node evaluates. */
export const buildPlutusMint = plutusMint;
