/**
 * Admission accepts only orders the node decodes. One case table drives both
 * sides: each row's order is built through the real event-history observer in
 * the emulator (one InsertOrder per row) and read the way the node reads an
 * admitted order (the follower's `openOrder`, the node's user-event entry, and
 * for a withdrawal its L1 addresses, L2 output reference ledger key and payout
 * and refund datums). The two verdicts must agree with each other and with the
 * row.
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
import { type Row, ROWS } from "./event-admission-agreement-emulator.rows.js";
import { withSdkReading } from "./event-admission-agreement-emulator.sdk-reading.js";
import { makeUserEventBuilderFixture } from "./reserve-payout-builders.make-user-event-builder-fixture.js";

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
  row: Pick<Row, "l2Datum" | "withdrawal"> = {},
) =>
  Effect.runPromise(
    group === "withdrawal"
      ? Effect.gen(function* () {
          const withdrawal = row.withdrawal ?? {};
          const address = yield* SDK.addressDataFromBech32(
            fixture.beneficiary.address,
          );
          return yield* SDK.prepareWithdrawalSubmissionProgram(
            fixture.lucid,
            fixture.contracts,
            {
              ...options,
              body: {
                l2_outref: {
                  transactionId: "33".repeat(32),
                  outputIndex: withdrawal.outputIndex ?? 0n,
                },
                l2_owner: "44".repeat(28),
                l2_value: SDK.assetsToValue(
                  withdrawal.l2Assets ?? { lovelace: 7_000_000n },
                ),
                l1_address: address,
                l1_datum: withdrawal.l1Datum ?? "NoDatum",
              },
              refundAddress: address,
              refundDatum: withdrawal.refundDatum ?? "NoDatum",
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
            row.l2Datum ?? (group === "deposit" ? null : EXTERNAL_L2_DATUM),
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
      prepare(fixture, row.group, options, row),
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
    prepare(fixture, row.group, { ...options, ...external }, row),
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

  /** The chain's verdict on the row and the node's read of the same order. */
  const verdicts = async (row: Row) => {
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
    return {
      chain: chain.admitted,
      node: nodeDecodes(fixture, row.group, order, retained),
    };
  };

  it.each(ROWS)(
    "$group with $name: admitted $admitted",
    async (row) => {
      const { chain, node } = await verdicts(row);
      expect({ chain, node }).toEqual({
        chain: row.admitted,
        node: row.admitted,
      });
    },
    120_000,
  );

  it("external deposit with a datum nested to the deployed node bound: admitted true", async () => {
    const bound = SDK.requireEventHistoryContracts(fixture.contracts).deposit
      .recipe.maxPayloadNodes;
    // The payload's nodes besides a one-node datum, from the SDK's own
    // payload for this deposit.
    const shallow = await prepare(
      fixture,
      "deposit",
      { nonceInput: nonce },
      {
        l2Datum: "00",
      },
    );
    const others = SDK.countHistoryDataNodesCbor(shallow.plan.payloadCbor) - 1n;
    const depth = Number(bound - others) - 1;
    // `depth` nested lists around one integer.
    const l2Datum = `${"9f".repeat(depth)}00${"ff".repeat(depth)}`;
    const deep = await prepare(
      fixture,
      "external deposit",
      { nonceInput: nonce },
      {
        l2Datum,
      },
    );
    expect(SDK.countHistoryDataNodesCbor(deep.plan.payloadCbor)).toBe(bound);
    expect(
      await verdicts({
        group: "external deposit",
        name: "a deepest datum",
        admitted: true,
        l2Datum,
      }),
    ).toMatchObject({ chain: true, node: true });
  }, 240_000);
});
