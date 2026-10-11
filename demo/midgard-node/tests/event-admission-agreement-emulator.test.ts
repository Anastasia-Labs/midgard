/**
 * Admission accepts only orders the node decodes. One case table drives both
 * sides: each row's order is built through the real event-history observer in
 * the emulator (one InsertOrder per row) and decoded the way the node decodes
 * an admitted order (the follower's `openOrder`, the node's user-event entry
 * and its L1 address mapping). The two verdicts must agree with each other and
 * with the row.
 */
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  CML,
  Constr,
  Data,
  datumToHash,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  type Fixture,
  type Group,
  nodeDecodes,
} from "./event-admission-agreement-emulator.node-decodes.js";
import {
  type Swap,
  withSdkReading,
} from "./event-admission-agreement-emulator.sdk-reading.js";
import { makeUserEventBuilderFixture } from "./reserve-payout-builders.make-user-event-builder-fixture.js";

type Row = Readonly<{
  group: Group;
  name: string;
  admitted: boolean;
  /** Field replacements in the payload, by constructor-field path. */
  fields?: readonly (readonly [readonly number[], string])[];
  /** The external payload's reclamation credential. */
  reclaimAuth?: SDK.CredentialD;
  swap?: Swap;
}>;

const hash28 = "a7".repeat(28);
const shortHash: Swap = { short: "a5".repeat(27), padded: "a5".repeat(28) };
const shortTxId: Swap = { short: "a6".repeat(31), padded: "a6".repeat(32) };
const key = (hash: string) => Data.to(new Constr(0, [hash]));
const script = (hash: string) => Data.to(new Constr(1, [hash]));
const noStake = Data.to(new Constr(1, []));
const inlineStake = (hash: string) =>
  Data.to(new Constr(0, [new Constr(0, [new Constr(0, [hash])])]));
/** An address with a pointer stake credential, in the SDK's (ledger) shape. */
const pointerAddress = Data.to(
  {
    paymentCredential: { PublicKeyCredential: [hash28] },
    stakeCredential: {
      Pointer: { slotNumber: 1n, transactionIndex: 2n, certificateIndex: 3n },
    },
  },
  SDK.AddressData,
);

// Payload paths. Deposit: payload.event.info.l2_address = [0, 1, 0].
// Withdrawal: payload.event.info.body = [0, 1, 0]; refund_address = [1].
const L2_ADDRESS = [0, 1, 0];
const BODY = [0, 1, 0];
const L1_ADDRESS = [...BODY, 3];
const REFUND_ADDRESS = [1];

/** Every order shape admission must refuse because the node cannot decode it,
 * each next to an accepted control that differs from it in one field. */
const ROWS: readonly Row[] = [
  { group: "deposit", name: "the SDK's deposit", admitted: true },
  {
    group: "deposit",
    name: "a script payment credential",
    admitted: true,
    fields: [[[...L2_ADDRESS, 0], script(hash28)]],
  },
  {
    group: "deposit",
    name: "no stake credential",
    admitted: true,
    fields: [[[...L2_ADDRESS, 1], noStake]],
  },
  {
    group: "deposit",
    name: "a 27-byte payment key hash",
    admitted: false,
    fields: [[[...L2_ADDRESS, 0], key(shortHash.short)]],
    swap: shortHash,
  },
  {
    group: "deposit",
    name: "a 27-byte inline stake key hash",
    admitted: false,
    fields: [[[...L2_ADDRESS, 1], inlineStake(shortHash.short)]],
    swap: shortHash,
  },
  {
    group: "deposit",
    name: "a pointer stake credential",
    admitted: false,
    fields: [[L2_ADDRESS, pointerAddress]],
  },
  { group: "withdrawal", name: "the SDK's withdrawal", admitted: true },
  {
    group: "withdrawal",
    name: "an L1 address without stake credential",
    admitted: true,
    fields: [[[...L1_ADDRESS, 1], noStake]],
  },
  {
    group: "withdrawal",
    name: "a script refund credential",
    admitted: true,
    fields: [[[...REFUND_ADDRESS, 0], script(hash28)]],
  },
  {
    group: "withdrawal",
    name: "a 31-byte L2 transaction id",
    admitted: false,
    fields: [[[...BODY, 0, 0], Data.to(shortTxId.short)]],
    swap: shortTxId,
  },
  {
    group: "withdrawal",
    name: "a 27-byte L2 owner",
    admitted: false,
    fields: [[[...BODY, 1], Data.to(shortHash.short)]],
    swap: shortHash,
  },
  {
    group: "withdrawal",
    name: "a 27-byte L1 payment key hash",
    admitted: false,
    fields: [[[...L1_ADDRESS, 0], key(shortHash.short)]],
    swap: shortHash,
  },
  {
    group: "withdrawal",
    name: "a 27-byte L1 inline stake key hash",
    admitted: false,
    fields: [[[...L1_ADDRESS, 1], inlineStake(shortHash.short)]],
    swap: shortHash,
  },
  {
    group: "withdrawal",
    name: "a pointer L1 stake credential",
    admitted: false,
    fields: [[L1_ADDRESS, pointerAddress]],
  },
  {
    group: "withdrawal",
    name: "a 27-byte refund payment script hash",
    admitted: false,
    fields: [[[...REFUND_ADDRESS, 0], script(shortHash.short)]],
    swap: shortHash,
  },
  {
    group: "withdrawal",
    name: "a 27-byte refund inline stake key hash",
    admitted: false,
    fields: [[[...REFUND_ADDRESS, 1], inlineStake(shortHash.short)]],
    swap: shortHash,
  },
  {
    group: "withdrawal",
    name: "a pointer refund stake credential",
    admitted: false,
    fields: [[REFUND_ADDRESS, pointerAddress]],
  },
  { group: "external deposit", name: "the SDK's payload", admitted: true },
  {
    group: "external deposit",
    name: "a script reclamation credential",
    admitted: true,
    reclaimAuth: { ScriptCredential: [hash28] },
  },
  {
    group: "external deposit",
    name: "a 27-byte reclamation key hash",
    admitted: false,
    reclaimAuth: { PublicKeyCredential: [shortHash.short] },
    swap: shortHash,
  },
];

/** The emulator's clock moves with its blocks, not with wall time. */
const validity = (fixture: Fixture) => {
  const validFrom = fixture.lucid.slotToUnixTime(fixture.lucid.currentSlot());
  return {
    validFrom,
    validTo: SDK.resolveUserEventValidTo(
      fixture.lucid,
      undefined,
      () => validFrom,
    ),
  };
};

const depositConfig = (fixture: Fixture, l2Datum: string | null) => ({
  additionalAssets: {},
  l2Address: fixture.beneficiary.address,
  l2Datum,
  lovelace: 5_000_000n,
  referenceScripts: { depositMinting: fixture.depositMintingReference },
  validity: validity(fixture),
});

/** Large enough that the deposit's payload is retained externally. */
const EXTERNAL_L2_DATUM = Data.to("ab".repeat(600));

const prepare = (
  fixture: Fixture,
  group: Group,
  options: SDK.UserHistoryBuildOptions,
) =>
  Effect.runPromise(
    group === "withdrawal"
      ? Effect.gen(function* () {
          const address = yield* SDK.addressDataFromBech32(
            fixture.beneficiary.address,
          );
          return yield* SDK.prepareWithdrawalSubmissionProgram(
            fixture.lucid,
            fixture.contracts,
            {
              ...options,
              body: {
                l2_outref: { transactionId: "33".repeat(32), outputIndex: 0n },
                l2_owner: "44".repeat(28),
                l2_value: SDK.assetsToValue({ lovelace: 7_000_000n }),
                l1_address: address,
                l1_datum: "NoDatum",
              },
              refundAddress: address,
              referenceScripts: {
                withdrawalMinting: fixture.withdrawalMintingReference,
              },
              signature: ["01", "02"],
              validity: validity(fixture),
            },
          );
        })
      : SDK.prepareDepositSubmissionProgram(fixture.lucid, fixture.contracts, {
          ...depositConfig(
            fixture,
            group === "deposit" ? null : EXTERNAL_L2_DATUM,
          ),
          ...options,
        }),
  );

type ChainVerdict = Readonly<{ admitted: boolean; orderDatum?: string }>;

const errorText = (error: unknown): string =>
  error instanceof Error
    ? `${error.message} ${error.cause === undefined ? "" : errorText(error.cause)}`
    : String(error);

/** Builds the row's admission against the real observer. A build that fails
 * script evaluation of the history observer, the transaction's only
 * withdrawal, is a refusal; any other failure fails the test. */
const chainVerdict = async (
  build: () => Promise<{
    tx: { toCBOR: () => string };
    orderOutputIndex: number;
  }>,
): Promise<ChainVerdict> => {
  try {
    const built = await build();
    const output = CML.Transaction.from_cbor_hex(built.tx.toCBOR())
      .body()
      .outputs()
      .get(built.orderOutputIndex);
    const orderDatum = output.datum()?.as_datum()?.to_cbor_hex();
    if (orderDatum === undefined) throw new Error("order has no inline datum");
    return { admitted: true, orderDatum };
  } catch (error) {
    if (/failed script execution Withdraw\[0\]/u.test(errorText(error)))
      return { admitted: false };
    throw error;
  }
};

const withFields = (cbor: string, fields: Row["fields"]): string =>
  aikenSerialisedPlutusDataCborPreservingMapOrder(
    (fields ?? []).reduce(
      (current, [path, value]) =>
        replacePlutusConstrFieldCbor(current, [...path], value),
      cbor,
    ),
  );

/** The order output's datum field holding the inline payload, or the
 * external payload's storage hash. */
const LOCATION_FIELD = [3, 0, 2, 0];

type RowBuild = Readonly<{
  chain: ChainVerdict;
  /** The order datum field the row's payload, or its storage hash, fills. */
  location: string;
  prepared: Readonly<{ request: { assets: Assets } }>;
  retained: readonly UTxO[];
}>;

/** Builds the row's admission through the real observer, after publishing
 * its retention datum when the payload is external. Every row uses the same
 * nonce, which a publication never spends. */
const buildRow = async (
  fixture: Fixture,
  nonceInput: UTxO,
  row: Row,
): Promise<RowBuild> => {
  const options = {
    nonceInput,
    ...(row.reclaimAuth === undefined ? {} : { reclaimAuth: row.reclaimAuth }),
  };
  let retained: UTxO[] = [];
  if (row.group === "external deposit") {
    const publishing = await withSdkReading(row.swap, () =>
      prepare(fixture, row.group, options),
    );
    expect(publishing.plan.kind).toBe("External");
    const publication = await withSdkReading(row.swap, () =>
      SDK.buildEventHistoryPublication(
        publishing.context,
        publishing.plan.payloadCbor,
        publishing.request.reclaimAuth,
        validity(fixture).validTo,
      ),
    );
    const signed = await publication.tx.sign.withWallet().complete();
    const txHash = await signed.submit();
    await fixture.lucid.awaitTx(txHash);
    retained = await fixture.lucid.utxosByOutRef([
      { txHash, outputIndex: publication.publicationOutputIndex },
    ]);
  }
  const external =
    retained[0] === undefined ? {} : { externalData: retained[0] };
  const prepared = await withSdkReading(row.swap, () =>
    prepare(fixture, row.group, { ...options, ...external }),
  );
  const payloadCbor = withFields(prepared.request.payloadCbor, row.fields);
  const chain = await chainVerdict(() =>
    withSdkReading(row.swap, () =>
      SDK.buildEventHistoryAdmission(prepared.context, {
        ...prepared.request,
        payloadCbor,
        ...validity(fixture),
        ...external,
      }),
    ),
  );
  const location =
    retained[0] === undefined
      ? payloadCbor
      : Data.to(
          datumToHash(
            aikenSerialisedPlutusDataCborPreservingMapOrder(retained[0].datum!),
          ),
        );
  if (chain.orderDatum !== undefined)
    // The substitution a refused row's order uses reproduces what the
    // builder put on chain.
    expect(plutusConstrFieldCbor(chain.orderDatum, LOCATION_FIELD)).toBe(
      location,
    );
  return { chain, location, prepared, retained };
};

describe("event-history admission agrees with the node's decoder (emulator)", () => {
  let fixture: Fixture;
  let nonce: UTxO;
  const controls = new Map<Group, Promise<string>>();

  /** The group's admitted order with the SDK's own payload. */
  const controlOf = (group: Group): Promise<string> => {
    let control = controls.get(group);
    if (control === undefined) {
      control = buildRow(fixture, nonce, {
        group,
        name: "control",
        admitted: true,
      }).then(({ chain }) => {
        if (chain.orderDatum === undefined)
          throw new Error(`${group} control was refused`);
        return chain.orderDatum;
      });
      controls.set(group, control);
    }
    return control;
  };

  beforeAll(async () => {
    fixture = await makeUserEventBuilderFixture();
    // A publication needs funding inputs besides the nonce.
    const address = await fixture.lucid.wallet().address();
    const split = await fixture.lucid
      .newTx()
      .pay.ToAddress(address, { lovelace: 1_000_000_000n })
      .pay.ToAddress(address, { lovelace: 1_000_000_000n })
      .complete();
    const signed = await split.sign.withWallet().complete();
    await fixture.lucid.awaitTx(await signed.submit());
    nonce = (await fixture.lucid.wallet().getUtxos())[0]!;
  }, 120_000);

  it.each(ROWS)(
    "$group with $name: admitted $admitted",
    async (row) => {
      const { chain, location, prepared, retained } = await buildRow(
        fixture,
        nonce,
        row,
      );
      // The order the node reads: the admitted one, or for a refused row the
      // group's admitted control with only this row's payload (or its
      // external storage hash) in place.
      const orderDatum =
        chain.orderDatum ??
        replacePlutusConstrFieldCbor(
          await controlOf(row.group),
          LOCATION_FIELD,
          location,
        );
      const policyId = (
        row.group === "withdrawal"
          ? fixture.contracts.withdrawal
          : fixture.contracts.deposit
      ).policyId;
      const eventKey = datumToHash(
        Data.to(
          {
            transactionId: nonce.txHash,
            outputIndex: BigInt(nonce.outputIndex),
          },
          SDK.OutputReference,
        ),
      );
      const order: UTxO = {
        txHash: "00".repeat(32),
        outputIndex: 1,
        address: "",
        assets: { ...prepared.request.assets, [policyId + eventKey]: 1n },
        datum: orderDatum,
      };
      expect({
        chain: chain.admitted,
        node: nodeDecodes(fixture, row.group, order, retained),
      }).toEqual({ chain: row.admitted, node: row.admitted });
    },
    120_000,
  );
});
