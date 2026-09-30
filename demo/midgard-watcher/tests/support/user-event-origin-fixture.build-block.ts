import {
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborMapRaw,
  encodeCborUnsigned,
} from "@al-ft/midgard-core/codec/cbor";
import { computeHash32 } from "@al-ft/midgard-core/codec/hash";
import { CML } from "@lucid-evolution/lucid";

import { createWatcherLocalBackfillUserEventReferenceAuthority } from "../../src/indexers/user-event-reference-authority.js";
import { type WatcherLocalBackfillFinalityReceipt } from "../../src/l1/finality-engine.js";
import { type WatcherLocalBackfillObservationReceipt } from "../../src/l1/l1-adapter.js";
import { admitWatcherNativeRollForwardBlock } from "../../src/l1/native-block-admission.js";
import { WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION } from "../../src/l1/native-chain-sync.js";
import { type WatcherUserEventScriptBinding } from "../../src/runtime/deployment-identity.js";
import {
  makeConfig,
  makeOriginDeployment,
  type Point,
  pointAt,
  type SyntheticUserEventBlock,
} from "./user-event-origin-fixture.make-config.js";

export const buildBlock = (
  transactions: readonly string[],
  parentPoint: Point,
  slotInterval = 1n,
): SyntheticUserEventBlock => {
  const frames = transactions.map((cbor) =>
    CML.Transaction.from_cbor_hex(cbor),
  );
  const bodyParts = [
    encodeCborArrayRaw(frames.map((tx) => tx.body().to_cbor_bytes())),
    encodeCborArrayRaw(frames.map((tx) => tx.witness_set().to_cbor_bytes())),
    encodeCborMapRaw(
      frames.flatMap((tx, index) => {
        const auxiliary = tx.auxiliary_data();
        return auxiliary === undefined
          ? []
          : [
              [
                encodeCborUnsigned(BigInt(index)),
                auxiliary.to_cbor_bytes(),
              ] as const,
            ];
      }),
    ),
    encodeCborArrayRaw(
      frames.flatMap((tx, index) =>
        tx.is_valid() ? [] : [encodeCborUnsigned(BigInt(index))],
      ),
    ),
  ];
  const bodyHash = computeHash32(
    Buffer.concat(bodyParts.map((part) => computeHash32(part))),
  );
  const headerBody = CML.HeaderBody.new(
    BigInt(parentPoint.blockNo) + 1n,
    BigInt(parentPoint.slot) + slotInterval,
    CML.BlockHeaderHash.from_hex(parentPoint.blockHash),
    CML.PublicKey.from_bytes(new Uint8Array(32).fill(1)),
    CML.VRFVkey.from_raw_bytes(new Uint8Array(32).fill(2)),
    CML.VRFCert.new(new Uint8Array(64).fill(3), new Uint8Array(80).fill(4)),
    BigInt(bodyParts.reduce((size, part) => size + part.length, 0)),
    CML.BlockBodyHash.from_raw_bytes(bodyHash),
    CML.OperationalCert.new(
      CML.KESVkey.from_raw_bytes(new Uint8Array(32).fill(5)),
      0n,
      0n,
      CML.Ed25519Signature.from_raw_bytes(new Uint8Array(64).fill(6)),
    ),
    CML.ProtocolVersion.new(9n, 0n),
  );
  const header = CML.Header.new(
    headerBody,
    CML.KESSignature.from_cbor_bytes(
      encodeCborBytes(new Uint8Array(448).fill(7)),
    ),
  );
  const headerBytes = header.to_cbor_bytes();
  const point = pointAt(
    computeHash32(headerBytes).toString("hex"),
    headerBody.block_number(),
    headerBody.slot(),
  );
  const rawBlockCbor = encodeCborArrayRaw([headerBytes, ...bodyParts]).toString(
    "hex",
  );
  const nativeBlock = admitWatcherNativeRollForwardBlock({
    ...point,
    schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
    kind: "roll_forward",
    blockType: "7",
    prevHash: parentPoint.blockHash,
    rawBlockCbor,
    tip: { kind: "point", ...point },
  });
  if (
    nativeBlock.transactionIds.length !== frames.length ||
    frames.some(
      (tx, index) =>
        CML.hash_transaction(tx.body()).to_hex() !==
        nativeBlock.transactionIds[index],
    )
  )
    throw new Error("Synthetic block changed a transaction identity");
  return Object.freeze({ point, parentPoint, nativeBlock });
};

export type SyntheticFinalizedUserEventBlock = Readonly<{
  finality: WatcherLocalBackfillFinalityReceipt;
  observation: WatcherLocalBackfillObservationReceipt;
  referenceAuthority: ReturnType<
    typeof createWatcherLocalBackfillUserEventReferenceAuthority
  >;
  close: () => Promise<void>;
}>;

export type SyntheticNativeTip = Readonly<{
  blockHash: string;
  blockNo: string;
  slot: string;
}>;

export type SyntheticNativeQuery = Readonly<{
  startupDigest: string;
  target: SyntheticNativeTip;
  tip: SyntheticNativeTip;
}>;

export type OriginDeployment = Omit<
  ReturnType<typeof makeOriginDeployment>,
  "signedIdentity" | "trustRoots"
> & {
  signedIdentity: unknown;
  trustRoots: readonly ReturnType<
    typeof makeOriginDeployment
  >["trustRoots"][number][];
};

export type SyntheticUserEventOriginFixture = Readonly<{
  deployment: OriginDeployment;
  deploymentIdentity: ReturnType<typeof makeOriginDeployment>["result"];
  scriptBinding: WatcherUserEventScriptBinding;
  watcherConfig: ReturnType<typeof makeConfig>;
  nativeChainSyncBinaryPath: string;
  activationBlock: SyntheticUserEventBlock;
  emptySuccessorBlock: SyntheticUserEventBlock;
  activationTransactionCbor: string;
  initializationBodyCbor: string;
  makeBlock: (
    input: Readonly<{
      transactions: readonly string[];
      creatingBodies?: readonly string[];
      parent?: SyntheticUserEventBlock;
      slot?: number;
    }>,
  ) => Promise<SyntheticUserEventBlock>;
  openFinalizedBlock: (
    block: SyntheticUserEventBlock,
  ) => Promise<SyntheticFinalizedUserEventBlock>;
  /** Controlled mode only. Set the initial tip before starting a monitor. */
  setNativeTip: (tip: SyntheticNativeTip) => Promise<void>;
  /** Append to the current controlled tip and publish the new tip atomically. */
  appendNativeBlock: (
    input: Readonly<{ transactions: readonly string[]; slot: number }>,
  ) => Promise<SyntheticUserEventBlock>;
  /** Adds actual empty native frames descending from the current controlled tip. */
  growNativeTip: (
    count?: number,
    minimumFirstSlot?: number,
  ) => Promise<SyntheticNativeTip>;
  rollbackNativeStream: (point: SyntheticNativeTip | "origin") => Promise<void>;
  /** Select a registered branch; orphan exact-point queries and intersections fail. */
  selectCanonicalBranch: (tip: SyntheticNativeTip) => Promise<void>;
  exitNativeStream: (exitCode?: number) => Promise<void>;
  /** Descriptive acquisitions recorded by the actual helper subprocess. */
  readNativeQueries: () => Promise<readonly SyntheticNativeQuery[]>;
  close: () => Promise<void>;
}>;
