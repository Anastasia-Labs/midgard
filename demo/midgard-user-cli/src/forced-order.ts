/**
 * `midgard-user forced-order`: build one L2 transaction of a named shape from
 * the wallet's L2 outputs, screen it as the tx-order mint does, and, when the
 * screen admits it, mint a forced order carrying it on L1.
 *
 * A refused shape stops at the screen: the command names the check and
 * submits nothing. An admitted shape is minted with the SDK's tx-order
 * builder and broadcast once; the command then waits until the order is on
 * chain or the indexer has passed its TTL without it. There is no submission
 * record, so a rerun after an interrupted wait mints a second order.
 */
import {
  decodeMidgardForcedTxFullFromCanonicalCbor,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardForcedTxCanonical,
  submittedForcedTransactionFromNative,
} from "@al-ft/midgard-core/codec";
import { screenMidgardForcedTxAdmission } from "@al-ft/midgard-core/consensus-validation";
import { outRefLabel } from "@al-ft/midgard-core/out-ref";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, type UTxO } from "@lucid-evolution/lucid";
import { Effect, Either } from "effect";

import { type UserCliDeps } from "./deps.js";
import {
  type L1AccessOptions,
  requireKupoCoverage,
  resolveL1Access,
} from "./l1-view.js";
import { type L2CommandOptions, openL2Wallet, prepareL2Command } from "./l2.js";
import {
  FORCED_ORDER_SHAPE_NAMES,
  FORCED_ORDER_SHAPES,
  type ForcedOrderShape,
} from "./l2-shapes.js";
import { loadTxOrderDeployment } from "./manifest.js";
import { parseAddressData, refuse } from "./options.js";

export type ForcedOrderOptions = L2CommandOptions &
  L1AccessOptions & {
    readonly shape: string;
  };

export type ForcedOrderResult = {
  readonly shape: ForcedOrderShape;
  /** The L2 transaction's id, which the order's datum carries. */
  readonly l2TxId: string;
  /** The forced-form bytes the order commits to. */
  readonly forcedTxCbor: string;
  readonly orderTxHash: string;
  /** The order's event id: the L1 output its mint spent. */
  readonly txOrderId: string;
  readonly txOrderAddress: string;
  /** POSIX ms after which a block must include the forced transaction. */
  readonly inclusionTime: number;
};

const parseShape = (value: string): ForcedOrderShape =>
  (FORCED_ORDER_SHAPE_NAMES as readonly string[]).includes(value)
    ? (value as ForcedOrderShape)
    : refuse(
        `Unknown --shape "${value}"; one of ${FORCED_ORDER_SHAPE_NAMES.join(", ")}.`,
      );

/** The order's nonce: an ordinary wallet output, as the devnet journeys pick. */
const ordinaryWalletInput = (utxos: readonly UTxO[]): UTxO =>
  utxos.find(
    (utxo) =>
      utxo.datum == null &&
      utxo.datumHash == null &&
      utxo.scriptRef == null &&
      Object.keys(utxo.assets).every((unit) => unit === "lovelace"),
  ) ??
  refuse("The wallet has no ADA-only L1 output to spend as the order's nonce.");

const ttlOf = (signedCbor: string): number => {
  const ttl = CML.Transaction.from_cbor_hex(signedCbor).body().ttl();
  return ttl === undefined
    ? refuse("The built tx order has no TTL.")
    : Number(ttl);
};

export const runForcedOrder = async (
  options: ForcedOrderOptions,
  deps: UserCliDeps,
): Promise<ForcedOrderResult> => {
  const shape = parseShape(options.shape);
  const spec = FORCED_ORDER_SHAPES[shape];
  const { deployment, seedPhrase, wallet } = await prepareL2Command(
    options,
    deps,
    loadTxOrderDeployment,
  );
  const access = resolveL1Access(options, deps.env, deployment.network);
  const l2 = await openL2Wallet(deps, options.nodeEndpoint, deployment, wallet);
  const available = l2.shapeWallet.utxos.length;
  if (available < spec.inputs)
    refuse(
      `The ${shape} shape uses ${spec.inputs.toString()} of the wallet's L2 outputs; the node holds ${available.toString()} at ${wallet.address}.`,
    );

  const built = await spec.build(l2.midgard, l2.shapeWallet).complete();
  const signed = await built.sign.withWallet().complete();
  const forced = encodeMidgardForcedTxCanonical(
    submittedForcedTransactionFromNative(
      decodeMidgardNativeTxFullFromCanonicalCbor(
        Buffer.from(signed.txHex, "hex"),
      ),
    ),
  );
  const refusal = screenMidgardForcedTxAdmission(
    decodeMidgardForcedTxFullFromCanonicalCbor(forced),
  );
  if (refusal !== null)
    refuse(
      `The tx-order mint refuses the ${shape} transaction ${signed.txIdHex} at ${refusal.check}: ${refusal.detail}. Nothing was submitted.`,
    );
  if (spec.refusedAt !== null)
    refuse(
      `The tx-order mint's screen admits the ${shape} transaction ${signed.txIdHex}, which it should refuse at ${spec.refusedAt}. Nothing was submitted.`,
    );

  const contracts = {
    hubOracle: deployment.contracts.hubOracle,
    txOrder: deployment.txOrder,
  };
  if (access.kind === "kupmios")
    await requireKupoCoverage(deps.fetch, access.kupoUrl, [
      { role: "wallet address", address: wallet.address },
      {
        role: "tx-order address",
        address: contracts.txOrder.spendingScriptAddress,
      },
      {
        role: "hub oracle address",
        address: contracts.hubOracle.spendingScriptAddress,
      },
    ]);
  const view = await deps.openL1View({
    access,
    network: deployment.network,
    slotConfig: deployment.slotConfig,
    seedPhrase,
  });
  const order = await Effect.runPromise(
    Effect.either(
      SDK.buildUnsignedTxOrderTxWithMetadataProgram(view.lucid, contracts, {
        submittedTxCbor: forced.toString("hex"),
        nonceInput: ordinaryWalletInput(await view.lucid.wallet().getUtxos()),
        refundAddress: parseAddressData(wallet.address, "wallet address"),
        now: deps.now,
      }),
    ),
  );
  if (Either.isLeft(order))
    return refuse(
      `Cannot build the tx order: ${order.left.message}${order.left.cause === undefined ? "" : ` (${String(order.left.cause)})`}`,
    );
  const { tx, metadata } = order.right;
  const signedOrder = await tx.sign.withWallet().complete();
  const orderTxHash = signedOrder.toHash();
  const ttlSlot = ttlOf(signedOrder.toCBOR());
  await signedOrder.submit();
  deps.stderr(`broadcast forced order ${orderTxHash}\n`);
  for (;;) {
    // The indexed slot first: an exact-hash answer read after it covers every
    // block up to that slot.
    const indexed = await view.indexedSlot();
    const status = await SDK.observeUserSubmissionAttempt(
      view.lucid,
      orderTxHash,
    );
    if (status.status === "confirmed") break;
    if (status.status !== "pending" && indexed >= ttlSlot)
      refuse(
        `The forced order ${orderTxHash} expired unseen at slot ${ttlSlot.toString()}; nothing landed.`,
      );
    await deps.sleep(SDK.USER_SUBMISSION_POLL_INTERVAL_MS);
  }
  return {
    shape,
    l2TxId: signed.txIdHex,
    forcedTxCbor: forced.toString("hex"),
    orderTxHash,
    txOrderId: outRefLabel(metadata.nonceInput),
    txOrderAddress: metadata.txOrderAddress,
    inclusionTime: metadata.inclusionTime,
  };
};

export const formatForcedOrder = (result: ForcedOrderResult): string =>
  `forced order ${result.shape} confirmed: L2 tx ${result.l2TxId}, order ${result.orderTxHash} (event ${result.txOrderId}), due by ${new Date(result.inclusionTime).toISOString()}\n`;
