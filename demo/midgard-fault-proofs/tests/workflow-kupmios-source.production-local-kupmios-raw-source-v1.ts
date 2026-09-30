import "./workflow-kupmios-source.concrete-kupmios-transport-cancellation-and-response-bounds.js";

import { readFile } from "node:fs/promises";
import { resolve } from "node:path";

import {
  CML,
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  admitKupoMatchAgainstTransactionOutput,
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Point,
  isLocalKupmiosPointBehindKupoHead,
  LocalKupmiosExactPointNotCanonicalError,
  OGMIOS_RAW_TRANSACTION_CBOR_FLAG,
  pinAdmittedLocalKupmiosBoundaryAtPoint,
  readAdmittedLocalKupmiosAddressUtxosAtPoint,
  readAdmittedLocalKupmiosBoundary,
  readAdmittedLocalKupmiosPredecessorPoint,
  readAdmittedLocalKupmiosRawBlockAtPoint,
  readAdmittedLocalKupmiosRawTransaction,
  readAdmittedLocalKupmiosReferenceBodiesAtPoint,
  readAdmittedLocalKupmiosTransactionInclusion,
  readAdmittedLocalKupmiosUnitHistoryAtPoint,
  readAdmittedLocalKupmiosUtxosByOutRefAtPoint,
  requireOgmiosRawTransactionCbor,
} from "../src/workflow/index.js";
import {
  ANCESTOR,
  chainPoint,
  EARLIER,
  hash,
  ordinaryTransaction,
  TARGET,
} from "./workflow-kupmios-source.ogmios-boundary-socket.js";
import { sourceFixture } from "./workflow-kupmios-source.source-fixture.js";

describe("production local Kupmios raw source V1", () => {
  it("pins real Kupo checkpoint headers and requests string asset quantities", async () => {
    const fixture = sourceFixture();
    const boundary = await readAdmittedLocalKupmiosBoundary({
      source: fixture.source,
    });
    expect(boundary.kupoCheckpoint).toEqual({
      slot: "400",
      blockHash: TARGET,
      blockNo: "71",
      pointId: computeFraudProofRawL1PointId({
        slot: "400",
        blockHash: TARGET,
        blockNo: "71",
      }),
    });
    expect(boundary.confirmationDepth).toBe(30);
    await expect(
      readAdmittedLocalKupmiosBoundary({ source: { ...fixture.source } }),
    ).rejects.toThrow(/requires the admitted local Kupo\/Ogmios source/u);
    await expect(
      fixture.source.scanAddressPage({
        address:
          "addr_test1wzj2e2d2x6ns5w50z3h2zlaurqu4h9tpuv7zpkg6dj6xefcp4x24g",
        throughPoint: boundary.kupoCheckpoint,
        after: null,
      }),
    ).resolves.toMatchObject({ complete: true, nextCursor: null, utxos: [] });
    const matchRequest = fixture.requests.find(({ url }) =>
      url.includes("/matches/"),
    );
    expect(matchRequest?.url).toContain("resolve_hashes&order=oldest_first");
    expect(new Headers(matchRequest?.init?.headers).get("accept")).toBe(
      "application/json;asset-quantity=string",
    );
  });

  it.each([3_000_000n, 9_007_199_254_740_993n])(
    "preserves exact numeric Kupo lovelace %s through raw CBOR admission",
    async (coins) => {
      const address = credentialToAddress(
        "Preprod",
        scriptHashToCredential("31".repeat(28)),
      );
      const output = CML.TransactionOutput.new(
        CML.Address.from_bech32(address),
        CML.Value.from_coin(coins),
      );
      const outputs = CML.TransactionOutputList.new();
      outputs.add(output);
      const body = CML.TransactionBody.new(
        CML.TransactionInputList.new(),
        outputs,
        0n,
      );
      const transaction = CML.Transaction.new(
        body,
        CML.TransactionWitnessSet.new(),
        true,
      );
      const id = CML.hash_transaction(body).to_hex();
      const match = {
        transaction_index: 0,
        transaction_id: id,
        output_index: 0,
        address,
        value: { coins, assets: {} },
        datum_hash: null,
        script_hash: null,
        created_at: { slot_no: 400, header_hash: TARGET },
        spent_at: null,
        datum: null,
        script: null,
      };
      const fixture = sourceFixture({
        blockTransactions: [{ id, cbor: transaction.to_canonical_cbor_hex() }],
        kupoMatches: [match],
      });
      const boundary = await readAdmittedLocalKupmiosBoundary({
        source: fixture.source,
      });
      await expect(
        fixture.source.scanAddressPage({
          address,
          throughPoint: boundary.kupoCheckpoint,
          after: null,
        }),
      ).resolves.toMatchObject({
        utxos: [{ outputCbor: output.to_canonical_cbor_hex() }],
      });
      if (coins > BigInt(Number.MAX_SAFE_INTEGER)) {
        expect(() =>
          admitKupoMatchAgainstTransactionOutput({
            match: { ...match, value: { coins: Number(coins), assets: {} } },
            outputCbor: output.to_canonical_cbor_hex(),
          }),
        ).toThrow("exact nonnegative quantity");
      }
    },
  );

  it("binds Kupo value and reference-script identity to raw output CBOR", () => {
    const script = CML.Script.new_plutus_v3(
      CML.PlutusV3Script.from_raw_bytes(Uint8Array.from([1, 2, 3])),
    );
    const address = credentialToAddress(
      "Preview",
      scriptHashToCredential("31".repeat(28)),
    );
    const output = CML.TransactionOutput.new(
      CML.Address.from_bech32(address),
      CML.Value.from_coin(3_000_000n),
      undefined,
      script,
    );
    const outputScript = output.script_ref()!;
    const match = {
      transaction_index: 0,
      transaction_id: hash(7),
      output_index: 0,
      address,
      value: { coins: "3000000", assets: {} },
      datum_hash: null,
      script_hash: outputScript.hash().to_hex(),
      created_at: { slot_no: 390, header_hash: hash(8) },
      spent_at: null,
      datum: null,
      script: {
        language: "plutus:v3",
        script: Buffer.from(
          outputScript.as_plutus_v3()!.to_raw_bytes(),
        ).toString("hex"),
      },
    };
    expect(() =>
      admitKupoMatchAgainstTransactionOutput({
        match,
        outputCbor: output.to_canonical_cbor_hex(),
      }),
    ).not.toThrow();
    expect(() =>
      admitKupoMatchAgainstTransactionOutput({
        match: { ...match, value: { coins: "2999999", assets: {} } },
        outputCbor: output.to_canonical_cbor_hex(),
      }),
    ).toThrow(/value disagrees/u);
    expect(() =>
      admitKupoMatchAgainstTransactionOutput({
        match: {
          ...match,
          script: { language: "plutus:v3", script: "09" },
        },
        outputCbor: output.to_canonical_cbor_hex(),
      }),
    ).toThrow(/reference script disagrees/u);
  });

  it.each(["d8798101", "d8799f01ff"])(
    "binds inline datum identity to its original ledger bytes (%s)",
    (datumCbor) => {
      const datum = CML.PlutusData.from_cbor_hex(datumCbor);
      const address = credentialToAddress(
        "Preprod",
        scriptHashToCredential("31".repeat(28)),
      );
      const output = CML.TransactionOutput.new(
        CML.Address.from_bech32(address),
        CML.Value.from_coin(3_000_000n),
        CML.DatumOption.new_datum(datum),
      );
      const match = {
        transaction_index: 0,
        transaction_id: hash(7),
        output_index: 0,
        address,
        value: { coins: "3000000", assets: {} },
        datum_hash: CML.hash_plutus_data(datum).to_hex(),
        datum_type: "inline",
        script_hash: null,
        created_at: { slot_no: 390, header_hash: hash(8) },
        spent_at: null,
        datum: datumCbor,
        script: null,
      };
      const admit = (candidate: typeof match) =>
        admitKupoMatchAgainstTransactionOutput({
          match: candidate,
          outputCbor: output.to_cbor_hex(),
        });
      expect(() => admit(match)).not.toThrow();
      expect(() => admit({ ...match, datum_hash: hash(0xff) })).toThrow(
        /inline datum disagrees/u,
      );
      expect(() => admit({ ...match, datum: "d8798102" })).toThrow(
        /inline datum disagrees/u,
      );
      const reencoded = datumCbor === "d8798101" ? "d8799f01ff" : "d8798101";
      expect(() =>
        admit({
          ...match,
          datum: reencoded,
          datum_hash: CML.hash_plutus_data(
            CML.PlutusData.from_cbor_hex(reencoded),
          ).to_hex(),
        }),
      ).toThrow(/inline datum disagrees/u);
    },
  );

  it("fails closed when Ogmios omits raw transaction CBOR", () => {
    expect(() =>
      requireOgmiosRawTransactionCbor({
        value: { id: hash(9) },
        expectedTxHash: hash(9),
        label: "transaction",
      }),
    ).toThrow(new RegExp(OGMIOS_RAW_TRANSACTION_CBOR_FLAG, "u"));
  });

  it("rejects an oversized provider response before buffering it", async () => {
    const fixture = sourceFixture({ oversizedKupo: true });
    await expect(fixture.source.readBoundary()).rejects.toThrow(
      /exceeds the raw-source byte bound/u,
    );
  });

  it("accepts only transaction CBOR whose body hashes to the reported id", () => {
    const body = CML.TransactionBody.new(
      CML.TransactionInputList.new(),
      CML.TransactionOutputList.new(),
      0n,
    );
    const transaction = CML.Transaction.new(
      body,
      CML.TransactionWitnessSet.new(),
      true,
    );
    const txHash = CML.hash_transaction(body).to_hex();
    expect(
      requireOgmiosRawTransactionCbor({
        value: { id: txHash, cbor: transaction.to_canonical_cbor_hex() },
        expectedTxHash: txHash,
        label: "transaction",
      }),
    ).toBe(transaction.to_canonical_cbor_hex());
  });

  it("reports whether a non-canonical exact point is merely ahead of Kupo's head", async () => {
    const fixture = sourceFixture();
    await fixture.source.readBoundary();
    const lagging = await readAdmittedLocalKupmiosRawBlockAtPoint({
      source: fixture.source,
      point: chainPoint("1200", hash(0x21), "90"),
    }).then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(lagging).toBeInstanceOf(LocalKupmiosExactPointNotCanonicalError);
    expect(
      (lagging as LocalKupmiosExactPointNotCanonicalError).kupoLag,
    ).toEqual({ requestedSlot: 1200, checkpointSlot: 400, kupoHeadSlot: 990 });
    expect(isLocalKupmiosPointBehindKupoHead(lagging)).toBe(true);
    const diverged = await readAdmittedLocalKupmiosRawBlockAtPoint({
      source: fixture.source,
      point: chainPoint("400", hash(0x22), "71"),
    }).then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(diverged).toBeInstanceOf(LocalKupmiosExactPointNotCanonicalError);
    expect(
      (diverged as LocalKupmiosExactPointNotCanonicalError).kupoLag,
    ).toEqual({ requestedSlot: 400, checkpointSlot: 400, kupoHeadSlot: 990 });
    expect(isLocalKupmiosPointBehindKupoHead(diverged)).toBe(false);
    expect(isLocalKupmiosPointBehindKupoHead(new Error("other"))).toBe(false);
    expect((lagging as Error).message).toContain(
      "requested slot 1200, checkpoint slot 400, Kupo head slot 990",
    );
    // Kupo's head header can already name the requested block while the
    // checkpoint query still resolves to its predecessor; that is lag.
    const raced = new LocalKupmiosExactPointNotCanonicalError("raced", {
      requestedSlot: 1200,
      checkpointSlot: 1190,
      kupoHeadSlot: 1200,
    });
    expect(isLocalKupmiosPointBehindKupoHead(raced)).toBe(true);
    const forked = new LocalKupmiosExactPointNotCanonicalError("forked", {
      requestedSlot: 1200,
      checkpointSlot: 1200,
      kupoHeadSlot: 1300,
    });
    expect(isLocalKupmiosPointBehindKupoHead(forked)).toBe(false);
  });

  it("re-admits an exact ordered raw block only from the opaque concrete source", async () => {
    const first = ordinaryTransaction(1n);
    const second = ordinaryTransaction(2n);
    const fixture = sourceFixture({
      blockTransactions: [first, second],
    });
    const boundary = (await fixture.source.readBoundary()) as {
      readonly kupoCheckpoint: {
        readonly slot: string;
        readonly blockHash: string;
        readonly blockNo: string;
        readonly pointId: string;
      };
    };
    await expect(
      readAdmittedLocalKupmiosRawBlockAtPoint({
        source: fixture.source,
        point: boundary.kupoCheckpoint,
      }),
    ).resolves.toMatchObject({
      sourceId: fixture.source.sourceId,
      point: boundary.kupoCheckpoint,
      kupoCheckpoint: { slot: 400, blockHash: TARGET },
      transactions: [
        { txHash: first.id, transactionCbor: first.cbor },
        { txHash: second.id, transactionCbor: second.cbor },
      ],
    });
    const references = await readAdmittedLocalKupmiosReferenceBodiesAtPoint({
      source: fixture.source,
      point: boundary.kupoCheckpoint,
    });
    expect(references.targetBlock.transactions).toEqual([
      { txHash: first.id, transactionCbor: first.cbor },
      { txHash: second.id, transactionCbor: second.cbor },
    ]);
    expect(references.creatingTransactionBodies).toEqual([]);
    const rolledBackBlockHash = hash(0xec);
    await expect(
      readAdmittedLocalKupmiosRawBlockAtPoint({
        source: fixture.source,
        point: {
          ...boundary.kupoCheckpoint,
          blockHash: rolledBackBlockHash,
          pointId: computeFraudProofRawL1PointId({
            ...boundary.kupoCheckpoint,
            blockHash: rolledBackBlockHash,
          }),
        },
      }),
    ).rejects.toBeInstanceOf(LocalKupmiosExactPointNotCanonicalError);
    await expect(
      readAdmittedLocalKupmiosRawBlockAtPoint({
        source: { ...fixture.source },
        point: boundary.kupoCheckpoint,
      }),
    ).rejects.toThrow(/requires the admitted local Kupo\/Ogmios source/u);
  });

  it.each([false, true])(
    "projects an exact predecessor with cached child: %s",
    async (cached) => {
      const transactions = [ordinaryTransaction(1n), ordinaryTransaction(2n)];
      const fixture = sourceFixture({ blockTransactions: transactions });
      const point = chainPoint();
      if (cached) {
        await readAdmittedLocalKupmiosRawBlockAtPoint({
          source: fixture.source,
          point,
        });
      }
      const projection = await readAdmittedLocalKupmiosPredecessorPoint({
        source: fixture.source,
        point,
      });
      expect(projection).toEqual({
        sourceId: fixture.source.sourceId,
        point,
        predecessorPoint: chainPoint("380", ANCESTOR, "70"),
      });
      expect(Object.isFrozen(projection)).toBe(true);
      expect(Object.isFrozen(projection.point)).toBe(true);
      expect(Object.isFrozen(projection.predecessorPoint)).toBe(true);
      expect(fixture.sockets).toHaveLength(2);
      expect(fixture.sockets.map(({ intersection }) => intersection)).toEqual([
        { slot: 380, id: ANCESTOR },
        { slot: 360, id: EARLIER },
      ]);
      await expect(
        readAdmittedLocalKupmiosPredecessorPoint({
          source: fixture.source,
          point,
        }),
      ).resolves.toEqual(projection);
      expect(fixture.sockets).toHaveLength(2);
      await expect(
        readAdmittedLocalKupmiosRawBlockAtPoint({
          source: fixture.source,
          point,
        }),
      ).resolves.toEqual({
        schemaVersion: "midgard-local-kupmios-raw-block-at-point-v1",
        sourceId: fixture.source.sourceId,
        point,
        parentBlockHash: ANCESTOR,
        kupoCheckpoint: { slot: 400, blockHash: TARGET },
        transactions: transactions.map(({ id, cbor }) => ({
          txHash: id,
          transactionCbor: cbor,
        })),
      });
    },
  );

  it("binds predecessor acquisition to the captured source readers and identity", async () => {
    const fixture = sourceFixture();
    const sourceId = fixture.source.sourceId;
    fixture.source.readBlockAtPoint = async () => {
      throw new Error(
        "public block method must not supply predecessor evidence",
      );
    };
    Object.defineProperty(fixture.source, "sourceId", { value: "substituted" });
    await expect(
      readAdmittedLocalKupmiosPredecessorPoint({
        source: fixture.source,
        point: chainPoint(),
      }),
    ).resolves.toEqual({
      sourceId,
      point: chainPoint(),
      predecessorPoint: chainPoint("380", ANCESTOR, "70"),
    });
  });

  it("refuses source copies and malformed points before predecessor acquisition", async () => {
    const fixture = sourceFixture();
    await expect(
      readAdmittedLocalKupmiosPredecessorPoint({
        source: { ...fixture.source },
        point: chainPoint(),
      }),
    ).rejects.toThrow(/requires the admitted local Kupo\/Ogmios source/u);
    await expect(
      readAdmittedLocalKupmiosPredecessorPoint({
        source: fixture.source,
        point: { ...chainPoint(), pointId: hash(0xee) },
      }),
    ).rejects.toThrow(/pointId does not commit/u);
    expect(fixture.requests).toHaveLength(0);
  });

  it.each([
    { point: chainPoint("400", hash(0xef)), canonicality: true },
    { point: chainPoint("400", TARGET, "72"), canonicality: false },
  ])(
    "refuses a different requested child point: $canonicality",
    async ({ point, canonicality }) => {
      const fixture = sourceFixture();
      const result = readAdmittedLocalKupmiosPredecessorPoint({
        source: fixture.source,
        point,
      });
      if (canonicality) {
        await expect(result).rejects.toBeInstanceOf(
          LocalKupmiosExactPointNotCanonicalError,
        );
      } else {
        await expect(result).rejects.toThrow(
          "Ogmios exact block point differs from the request",
        );
      }
    },
  );

  it.each([
    { childAncestor: hash(0xef), parentHeight: 70 },
    { childAncestor: ANCESTOR, parentHeight: 69 },
    { childAncestor: ANCESTOR, parentHeight: 71 },
  ])(
    "refuses a non-direct predecessor: $childAncestor/$parentHeight",
    async (metadata) => {
      const fixture = sourceFixture(metadata);
      const result = readAdmittedLocalKupmiosPredecessorPoint({
        source: fixture.source,
        point: chainPoint(),
      });
      await expect(result).rejects.toThrow(
        "local Kupmios blocks do not form a direct predecessor",
      );
      await expect(result).rejects.not.toBeInstanceOf(
        LocalKupmiosExactPointNotCanonicalError,
      );
    },
  );

  it.each([400, 401])(
    "refuses a non-earlier checkpoint in both acquisition paths: %s",
    async (slot) => {
      const override = (lookup: number) =>
        lookup === 399 ? { slot_no: slot, header_hash: hash(0xef) } : undefined;
      const fixture = sourceFixture({ checkpointOverride: override });
      await expect(
        readAdmittedLocalKupmiosRawBlockAtPoint({
          source: fixture.source,
          point: chainPoint(),
        }),
      ).rejects.toThrow("Kupo did not return an earlier ancestor checkpoint");
      await expect(
        readAdmittedLocalKupmiosPredecessorPoint({
          source: fixture.source,
          point: chainPoint(),
        }),
      ).rejects.toThrow("Kupo did not return an earlier ancestor checkpoint");
      expect(fixture.sockets).toHaveLength(0);
    },
  );

  it("refuses a genesis parent without inventing a predecessor point", async () => {
    const fixture = sourceFixture({ childAncestor: "genesis" });
    await expect(
      readAdmittedLocalKupmiosPredecessorPoint({
        source: fixture.source,
        point: chainPoint(),
      }),
    ).rejects.toThrow("local Kupmios child has no block predecessor");
    expect(fixture.sockets).toHaveLength(1);
  });

  it("refuses a parent that ceased to be canonical", async () => {
    const fixture = sourceFixture({
      checkpointOverride: (slot) =>
        slot === 380 ? { slot_no: 380, header_hash: hash(0xef) } : undefined,
    });
    await expect(
      readAdmittedLocalKupmiosPredecessorPoint({
        source: fixture.source,
        point: chainPoint(),
      }),
    ).rejects.toBeInstanceOf(LocalKupmiosExactPointNotCanonicalError);
  });

  it("preserves a changed response head during final canonical confirmation", async () => {
    let advanceHead = false;
    const fixture = sourceFixture({
      checkpointOverride: (slot) =>
        advanceHead && slot === 399
          ? { slot_no: 380, header_hash: ANCESTOR, headHash: hash(0xef) }
          : undefined,
    });
    await fixture.source.readBoundary();
    advanceHead = true;
    await expect(
      fixture.source.confirmCanonicalPoint({ point: chainPoint() }),
    ).rejects.toThrow(
      "Kupo advanced or rolled back during raw snapshot capture",
    );
  });

  it("propagates final canonical-read failure without inventing a rollback", async () => {
    let fail = false;
    const originalError = new Error(
      "ordinary final checkpoint transport failure",
    );
    const fixture = sourceFixture({
      beforeFetch: async (url) => {
        if (fail && url.endsWith("/checkpoints/399")) throw originalError;
      },
    });
    await fixture.source.readBoundary();
    fail = true;
    await expect(
      fixture.source.confirmCanonicalPoint({ point: chainPoint() }),
    ).rejects.toBe(originalError);
  });

  it("reports a successfully observed point mismatch as noncanonical", async () => {
    const behavior = { childHeight: 71 };
    const fixture = sourceFixture({ socketBehavior: behavior });
    await fixture.source.readBoundary();
    behavior.childHeight = 72;
    await expect(
      fixture.source.confirmCanonicalPoint({ point: chainPoint() }),
    ).resolves.toEqual({ canonical: false, point: chainPoint() });
  });

  it("preserves capture-head refusal during predecessor acquisition", async () => {
    const fixture = sourceFixture({
      checkpointOverride: (slot) =>
        slot === 380
          ? { slot_no: 380, header_hash: ANCESTOR, headHash: hash(0xef) }
          : undefined,
    });
    const result = readAdmittedLocalKupmiosPredecessorPoint({
      source: fixture.source,
      point: chainPoint(),
    });
    await expect(result).rejects.toThrow(
      "Kupo advanced or rolled back during raw snapshot capture",
    );
    await expect(result).rejects.not.toBeInstanceOf(
      LocalKupmiosExactPointNotCanonicalError,
    );
  });

  it.each([false, true])(
    "rechecks the child after parent acquisition with cached blocks: %s",
    async (cached) => {
      let refuse = false;
      let childReads = 0;
      const fixture = sourceFixture({
        checkpointOverride: (slot) =>
          refuse && slot === 400 && ++childReads === 3
            ? { slot_no: 400, header_hash: hash(0xef) }
            : undefined,
      });
      if (cached) {
        await readAdmittedLocalKupmiosPredecessorPoint({
          source: fixture.source,
          point: chainPoint(),
        });
      }
      refuse = true;
      const result = readAdmittedLocalKupmiosPredecessorPoint({
        source: fixture.source,
        point: chainPoint(),
      });
      await expect(result).rejects.toBeInstanceOf(
        LocalKupmiosExactPointNotCanonicalError,
      );
      await expect(result).rejects.toThrow(
        "Kupo rolled back during predecessor point capture",
      );
      expect(childReads).toBe(3);
      expect(fixture.sockets).toHaveLength(2);
    },
  );

  it.each([false, true])(
    "re-admits exact resolved transaction bytes from the concrete source (indefinite: %s)",
    async (indefinite) => {
      const address = credentialToAddress(
        "Preview",
        scriptHashToCredential("31".repeat(28)),
      );
      const outputs = CML.TransactionOutputList.new();
      outputs.add(
        CML.TransactionOutput.new(
          CML.Address.from_bech32(address),
          CML.Value.from_coin(3_000_000n),
        ),
      );
      const body = CML.TransactionBody.new(
        CML.TransactionInputList.new(),
        outputs,
        200_000n,
      );
      const bodyCbor = indefinite
        ? `bf${body.to_canonical_cbor_hex().slice(2)}ff`
        : body.to_canonical_cbor_hex();
      const witnessSetCbor = indefinite ? "bfff" : "a0";
      const transactionCbor = `84${bodyCbor}${witnessSetCbor}f5f6`;
      const txHash = CML.hash_transaction(
        CML.TransactionBody.from_cbor_hex(bodyCbor),
      ).to_hex();
      const kupoMatch = {
        transaction_index: 0,
        transaction_id: txHash,
        output_index: 0,
        address,
        value: { coins: "3000000", assets: {} },
        datum_hash: null,
        script_hash: null,
        created_at: { slot_no: 400, header_hash: TARGET },
        spent_at: null,
        datum: null,
        script: null,
      };
      const fixture = sourceFixture({
        blockTransactions: [{ id: txHash, cbor: transactionCbor }],
        kupoMatches: [kupoMatch],
      });
      const boundary = (await fixture.source.readBoundary()) as {
        readonly kupoCheckpoint: {
          readonly slot: string;
          readonly blockHash: string;
          readonly blockNo: string;
          readonly pointId: string;
        };
      };
      await expect(
        readAdmittedLocalKupmiosRawTransaction({
          source: fixture.source,
          txHash,
          expectedInclusionPoint: boundary.kupoCheckpoint,
          minimumConfirmationDepth: 30,
        }),
      ).resolves.toMatchObject({
        txHash,
        bodyCbor,
        witnessSetCbor,
        inclusionPoint: boundary.kupoCheckpoint,
        confirmationDepth: 30,
        resolvedInputs: [],
        resolvedReferenceInputs: [],
      });
      await expect(
        readAdmittedLocalKupmiosAddressUtxosAtPoint({
          source: fixture.source,
          address,
          point: boundary.kupoCheckpoint,
        }),
      ).resolves.toEqual([
        {
          outRef: `${txHash}#0`,
          outputCbor: outputs.get(0).to_canonical_cbor_hex(),
          datumCbor: null,
          referenceScriptCbor: null,
        },
      ]);
      await expect(
        readAdmittedLocalKupmiosTransactionInclusion({
          source: fixture.source,
          txHash,
        }),
      ).resolves.toEqual(boundary.kupoCheckpoint);
      await expect(
        pinAdmittedLocalKupmiosBoundaryAtPoint({
          source: fixture.source,
          point: boundary.kupoCheckpoint,
        }),
      ).resolves.toBeUndefined();
      await expect(
        readAdmittedLocalKupmiosUtxosByOutRefAtPoint({
          source: fixture.source,
          point: boundary.kupoCheckpoint,
          outRefs: [`${txHash}#0`],
        }),
      ).resolves.toMatchObject({
        outputs: [{ outRef: `${txHash}#0` }],
        spends: [],
      });
      const spent = sourceFixture({
        blockTransactions: [{ id: txHash, cbor: transactionCbor }],
        kupoMatches: [
          {
            ...kupoMatch,
            spent_at: {
              transaction_id: hash(19),
              input_index: 0,
              slot_no: 400,
              header_hash: TARGET,
            },
          },
        ],
      });
      await pinAdmittedLocalKupmiosBoundaryAtPoint({
        source: spent.source,
        point: boundary.kupoCheckpoint,
      });
      await expect(
        readAdmittedLocalKupmiosTransactionInclusion({
          source: spent.source,
          txHash,
        }),
      ).resolves.toEqual(boundary.kupoCheckpoint);
      await expect(
        readAdmittedLocalKupmiosUtxosByOutRefAtPoint({
          source: spent.source,
          point: boundary.kupoCheckpoint,
          outRefs: [`${txHash}#0`],
        }),
      ).resolves.toEqual({ outputs: [], spends: [] });
      await expect(
        readAdmittedLocalKupmiosTransactionInclusion({
          source: { ...fixture.source },
          txHash,
        }),
      ).rejects.toThrow("admitted local Kupo/Ogmios history");
      await expect(
        pinAdmittedLocalKupmiosBoundaryAtPoint({
          source: fixture.source,
          point: {
            ...boundary.kupoCheckpoint,
            blockNo: "72",
            pointId: computeFraudProofRawL1PointId({
              ...boundary.kupoCheckpoint,
              blockNo: "72",
            }),
          },
        }),
      ).rejects.toThrow("differs from canonical");
      await expect(
        readAdmittedLocalKupmiosUnitHistoryAtPoint({
          source: fixture.source,
          unit: `${"12".repeat(28)}aa`,
          point: boundary.kupoCheckpoint,
        }),
      ).resolves.toEqual({
        checkpoint: boundary.kupoCheckpoint,
        transactions: [{ txHash, inclusionPoint: boundary.kupoCheckpoint }],
      });
      await expect(
        readAdmittedLocalKupmiosUnitHistoryAtPoint({
          source: { ...fixture.source },
          unit: `${"12".repeat(28)}aa`,
          point: boundary.kupoCheckpoint,
        }),
      ).rejects.toThrow(/requires the admitted local Kupo\/Ogmios source/u);
      await expect(
        readAdmittedLocalKupmiosRawTransaction({
          source: { ...fixture.source },
          txHash,
          expectedInclusionPoint: boundary.kupoCheckpoint,
          minimumConfirmationDepth: 30,
        }),
      ).rejects.toThrow(/requires the admitted local Kupo\/Ogmios source/u);
      await expect(
        readAdmittedLocalKupmiosRawTransaction({
          source: fixture.source,
          txHash,
          expectedInclusionPoint: boundary.kupoCheckpoint,
          minimumConfirmationDepth: 31,
        }),
      ).rejects.toThrow(/below release finality/u);
    },
  );

  describe("reports an exact outref's spend only with its consuming transaction verified", () => {
    const address = credentialToAddress(
      "Preview",
      scriptHashToCredential("31".repeat(28)),
    );
    const output = () => {
      const outputs = CML.TransactionOutputList.new();
      outputs.add(
        CML.TransactionOutput.new(
          CML.Address.from_bech32(address),
          CML.Value.from_coin(3_000_000n),
        ),
      );
      return outputs;
    };
    const creating = CML.TransactionBody.new(
      CML.TransactionInputList.new(),
      output(),
      200_000n,
    );
    const createdHash = CML.hash_transaction(creating).to_hex();
    const outRef = `${createdHash}#0`;
    const consuming = (spent: string, isValid = true) => {
      const inputs = CML.TransactionInputList.new();
      inputs.add(
        CML.TransactionInput.new(CML.TransactionHash.from_hex(spent), 0n),
      );
      const body = CML.TransactionBody.new(inputs, output(), 180_000n);
      return {
        id: CML.hash_transaction(body).to_hex(),
        cbor: `84${body.to_canonical_cbor_hex()}a0${isValid ? "f5" : "f4"}f6`,
      };
    };
    const read = async (
      spender: { readonly id: string; readonly cbor: string },
      spentSlot = 400,
    ) => {
      const fixture = sourceFixture({
        blockTransactions: [
          {
            id: createdHash,
            cbor: `84${creating.to_canonical_cbor_hex()}a0f5f6`,
          },
          spender,
        ],
        kupoMatches: [
          {
            transaction_index: 0,
            transaction_id: createdHash,
            output_index: 0,
            address,
            value: { coins: "3000000", assets: {} },
            datum_hash: null,
            script_hash: null,
            created_at: { slot_no: 400, header_hash: TARGET },
            spent_at: {
              transaction_id: spender.id,
              input_index: 0,
              slot_no: spentSlot,
              header_hash: TARGET,
            },
            datum: null,
            script: null,
          },
        ],
      });
      const boundary = (await fixture.source.readBoundary()) as {
        readonly kupoCheckpoint: FraudProofRawL1Point;
      };
      await pinAdmittedLocalKupmiosBoundaryAtPoint({
        source: fixture.source,
        point: boundary.kupoCheckpoint,
      });
      return {
        point: boundary.kupoCheckpoint,
        read: await readAdmittedLocalKupmiosUtxosByOutRefAtPoint({
          source: fixture.source,
          point: boundary.kupoCheckpoint,
          outRefs: [outRef],
        }),
      };
    };

    it("yields a verified spend when a valid consuming transaction lists the outref", async () => {
      const spender = consuming(createdHash);
      const { point, read: observed } = await read(spender);
      expect(observed).toEqual({
        outputs: [],
        spends: [{ outRef, spendingTxHash: spender.id, spendPoint: point }],
      });
    });

    it.each([
      ["does not list the outref", () => consuming("77".repeat(32))],
      ["is phase-2 invalid", () => consuming(createdHash, false)],
    ])(
      "yields no spend when the consuming transaction %s",
      async (_, spender) => {
        await expect(read(spender())).resolves.toMatchObject({
          read: { outputs: [], spends: [] },
        });
      },
    );

    it("does not report a spend above the read point: the outref reads as unspent", async () => {
      const { read: observed } = await read(consuming(createdHash), 401);
      expect(observed).toMatchObject({ outputs: [{ outRef }], spends: [] });
    });
  });

  it("keeps raw transaction CBOR enabled in every checked-in Ogmios launch path", async () => {
    const repository = resolve(process.cwd(), "../..");
    const paths = [
      "demo/midgard-node/scripts/run-ogmios.sh",
      "demo/midgard-node-tools/devnet/phase4-process/compose.yaml",
    ];
    for (const path of paths) {
      await expect(
        readFile(resolve(repository, path), "utf8"),
      ).resolves.toContain(OGMIOS_RAW_TRANSACTION_CBOR_FLAG);
    }
  });
});
