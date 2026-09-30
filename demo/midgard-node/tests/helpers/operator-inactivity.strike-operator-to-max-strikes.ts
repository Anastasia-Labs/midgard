import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  toUnit,
  type TxSignBuilder,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  alignedUnixTimeAtOrBefore,
  type OperatorInactivityFixture,
} from "./operator-inactivity.build-deployment-snapshot.js";
import {
  BUILDER_PREFLIGHT_MARKERS,
  fetchInactivityDirectorySnapshot,
  prepareInactivityStrike,
  requirePrimaryOperator,
  type StrikeAttemptOptions,
  submitInactivityStrike,
} from "./operator-inactivity.prepare-inactivity-strike.js";

/**
 * Attempts a strike that must be refused on chain, and returns the
 * evaluator's `failed script execution Spend[n] <reason>` text for the script
 * that refused it. Local UPLC evaluation runs the deployed validators while the
 * transaction is completed, so an on-chain refusal surfaces there; this helper
 * asserts the refusal really was a script failure and not one of the builder's
 * own pre-flight guards. The reason tells a failed `expect` (the validator
 * crashed) apart from a builtin failure such as a datum that did not decode.
 */
export const expectInactivityStrikeRefusal = async (
  fixture: OperatorInactivityFixture,
  options: StrikeAttemptOptions = {},
): Promise<string> => {
  const prepared = await prepareInactivityStrike(fixture, options);
  let message: string | undefined;
  try {
    await Effect.runPromise(
      SDK.buildStrikeInactiveOperatorTxProgram(prepared.config),
    );
  } catch (cause) {
    message = String(
      cause instanceof Error ? (cause.stack ?? cause.message) : cause,
    );
  }
  if (message === undefined) {
    throw new Error(
      "Expected the inactivity strike to be refused, but it completed",
    );
  }
  for (const marker of BUILDER_PREFLIGHT_MARKERS) {
    if (message.includes(marker)) {
      throw new Error(
        `Inactivity strike failed in the builder rather than on chain: ${message}`,
      );
    }
  }
  const scriptFailure = /failed script execution Spend\[\d+\][^"\n]*/.exec(
    message,
  );
  if (scriptFailure === null) {
    throw new Error(
      `Inactivity strike was rejected without a script execution failure: ${message}`,
    );
  }
  return scriptFailure[0];
};

/**
 * Drives the operator's node to `max_inactivity_strikes` by letting it miss
 * shift after shift. This is the state a forced retirement starts from.
 *
 * The shift only comes back round to one operator by rotating through all of
 * them, so in a multi-operator set every other operator is struck on the way
 * and ends up at the cap too. `txHashes` holds every strike submitted;
 * `inactivityStrikes` is the named operator's count.
 */
export const strikeOperatorToMaxStrikes = async (
  fixture: OperatorInactivityFixture,
  operatorKeyHash: string,
): Promise<{
  readonly txHashes: readonly string[];
  readonly inactivityStrikes: bigint;
}> => {
  const txHashes: string[] = [];
  const maxAttempts = 8 * Math.max(1, fixture.operators.length);
  for (let attempt = 0; attempt < maxAttempts; attempt += 1) {
    const snapshot = await fetchInactivityDirectorySnapshot(fixture);
    const node = SDK.findNodeByKey(snapshot.active, operatorKeyHash);
    if (node === undefined || node.active === null) {
      throw new Error(
        `Operator ${operatorKeyHash} has no active-operators node`,
      );
    }
    if (node.active.inactivity_strikes >= SDK.MAX_INACTIVITY_STRIKES) {
      return {
        txHashes,
        inactivityStrikes: node.active.inactivity_strikes,
      };
    }
    const current = SDK.schedulerCurrentOperator(snapshot.scheduler);
    if (current === null) {
      throw new Error("The scheduler holds no active operator");
    }
    const currentNode = SDK.findNodeByKey(snapshot.active, current.operator);
    if (currentNode?.active === undefined || currentNode.active === null) {
      throw new Error(
        `The scheduled operator ${current.operator} has no active-operators node`,
      );
    }
    if (currentNode.active.inactivity_strikes >= SDK.MAX_INACTIVITY_STRIKES) {
      throw new Error(
        `The shift is held by ${current.operator}, which is already at max_inactivity_strikes, so it cannot be advanced past by striking`,
      );
    }
    const submission = await submitInactivityStrike(fixture);
    txHashes.push(submission.txHash);
  }
  throw new Error(
    `Could not reach max_inactivity_strikes for ${operatorKeyHash}`,
  );
};

// ---------------------------------------------------------------------------
// Neglected user events
// ---------------------------------------------------------------------------

/**
 * The validity a neglected event's admission is built with.
 *
 * The SDK's default validity dates the event from the wall clock and opens its
 * range a minute in the past. Neither fits the emulator: its clock runs ahead
 * of the wall clock, and a range opening before the fixture's Lucid instance
 * was created falls below that instance's zero slot, which the evaluator
 * refuses as too far in the past. The range therefore opens at the emulator's
 * current time and closes where the SDK would close it from there.
 */
const neglectedEventValidity = (
  fixture: OperatorInactivityFixture,
): { readonly validFrom: number; readonly validTo: number } => {
  const emulatorNow = fixture.emulator.now();
  return {
    validFrom: Number(
      alignedUnixTimeAtOrBefore(fixture.lucid, BigInt(emulatorNow)),
    ),
    validTo: SDK.resolveUserEventValidTo(
      fixture.lucid,
      undefined,
      () => emulatorNow,
    ),
  };
};

const submitNeglectedEvent = async (
  fixture: OperatorInactivityFixture,
  kind: "Deposit" | "Withdrawal",
  built: {
    readonly tx: TxSignBuilder;
    readonly address: string;
    readonly authUnit: string;
    readonly inclusionTime: number;
  },
): Promise<SDK.NeglectedUserEventClaim> => {
  const signed = await built.tx.sign.withWallet().complete();
  const txHash = await signed.submit();
  await fixture.lucid.awaitTx(txHash);
  const [utxo] = await fixture.lucid.utxosAtWithUnit(
    built.address,
    built.authUnit,
  );
  if (utxo === undefined) {
    throw new Error(`The submitted ${kind} history node could not be found`);
  }
  return { kind, utxo, inclusionTimeMs: BigInt(built.inclusionTime) };
};

/**
 * Submits a deposit and returns its event-history Order node as a
 * neglected-user-event claim. The `inclusion_time` in the node's Order facts
 * is what the strike validator reads.
 */
export const submitNeglectedDeposit = async (
  fixture: OperatorInactivityFixture,
  lovelace = 20_000_000n,
): Promise<SDK.NeglectedUserEventClaim> => {
  const built = await Effect.runPromise(
    SDK.buildUnsignedDepositTxWithMetadataProgram(
      fixture.lucid,
      fixture.contracts,
      {
        l2Address: requirePrimaryOperator(fixture).address,
        l2Datum: null,
        lovelace,
        additionalAssets: {},
        validity: neglectedEventValidity(fixture),
      },
    ),
  );
  return submitNeglectedEvent(fixture, "Deposit", {
    tx: built.tx,
    address: built.metadata.depositAddress,
    authUnit: built.metadata.depositAuthUnit,
    inclusionTime: built.metadata.inclusionTime,
  });
};

/**
 * Submits a withdrawal order signed by the primary operator's key and returns
 * its event-history Order node as a neglected-user-event claim. The L2 output
 * it names need not exist: admission checks only the order's shape and
 * funding, and the strike reads only its Order facts.
 */
export const submitNeglectedWithdrawal = async (
  fixture: OperatorInactivityFixture,
  lovelace = 20_000_000n,
): Promise<SDK.NeglectedUserEventClaim> => {
  const primary = requirePrimaryOperator(fixture);
  const ownerKey = CML.PrivateKey.from_bech32(
    walletFromSeed(primary.seedPhrase, { network: "Custom" }).paymentKey,
  );
  const ownerAddress = await Effect.runPromise(
    SDK.addressDataFromBech32(primary.address),
  );
  const body: SDK.WithdrawalBody = {
    l2_outref: { transactionId: "ab".repeat(32), outputIndex: 0n },
    l2_owner: primary.keyHash,
    l2_value: SDK.assetsToValue({ lovelace }),
    l1_address: ownerAddress,
    l1_datum: "NoDatum",
  };
  const built = await Effect.runPromise(
    SDK.buildUnsignedWithdrawalTxWithMetadataProgram(
      fixture.lucid,
      fixture.contracts,
      {
        body,
        signature: SDK.signWithdrawalBody(ownerKey, body),
        refundAddress: ownerAddress,
        validity: neglectedEventValidity(fixture),
      },
    ),
  );
  return submitNeglectedEvent(fixture, "Withdrawal", {
    tx: built.tx,
    address: built.metadata.withdrawalAddress,
    authUnit: built.metadata.withdrawalAuthUnit,
    inclusionTime: built.metadata.inclusionTime,
  });
};

/**
 * Pays a copy of a neglected event's history node to the same list address
 * with the same inline datum but without the list's NFT: an output anyone can
 * create, which only the token bundle tells apart from the real node.
 */
export const submitUnauthenticatedHistoryNodeCopy = async (
  fixture: OperatorInactivityFixture,
  claim: SDK.NeglectedUserEventClaim,
): Promise<SDK.NeglectedUserEventClaim> => {
  const datum = claim.utxo.datum;
  if (datum === undefined || datum === null) {
    throw new Error("The history node carries no inline datum to copy");
  }
  const tx = await fixture.lucid
    .newTx()
    .pay.ToContract(
      claim.utxo.address,
      { kind: "inline", value: datum },
      { lovelace: claim.utxo.assets.lovelace },
    )
    .complete();
  const signed = await tx.sign.withWallet().complete();
  const txHash = await signed.submit();
  await fixture.lucid.awaitTx(txHash);
  const copies = await fixture.lucid.utxosByOutRef([
    { txHash, outputIndex: 0 },
  ]);
  const copy = copies[0];
  if (
    copy === undefined ||
    copy.address !== claim.utxo.address ||
    copy.datum !== datum ||
    Object.keys(copy.assets).length !== 1
  ) {
    throw new Error("The unauthenticated history node copy was not created");
  }
  return { ...claim, utxo: copy };
};

export const activeOperatorNodeUnit = (
  contracts: SDK.MidgardValidators,
  operatorKeyHash: string,
): string =>
  toUnit(
    contracts.activeOperators.policyId,
    SDK.ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX + operatorKeyHash,
  );
