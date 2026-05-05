import { describe, expect, it } from "vitest";
import { CML } from "@lucid-evolution/lucid";
import {
  encodeMidgardTxOutput,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  MidgardNodeProvider,
  outRefToCbor,
  ProviderCapabilityError,
  ProviderHttpError,
  ProviderPayloadError,
  ProviderTransportError,
  type MidgardFetch,
  type MidgardProtocolInfo,
  type OutRef,
} from "../src/index.js";

const address =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

const outRef: OutRef = {
  txHash: "11".repeat(32),
  outputIndex: 0,
};

const protocolInfo: MidgardProtocolInfo = {
  apiVersion: 1,
  network: "Preview",
  midgardNativeTxVersion: 1,
  currentSlot: 123456n,
  supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  protocolFeeParameters: {
    minFeeA: 44n,
    minFeeB: 155381n,
  },
  submissionLimits: {
    maxSubmitTxCborBytes: 32768,
  },
  validation: {
    strictnessProfile: "phase1_midgard",
    localValidationIsAuthoritative: false,
  },
};

const protocolInfoJson = {
  apiVersion: 1,
  network: "Preview",
  midgardNativeTxVersion: 1,
  currentSlot: "123456",
  supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
  protocolFeeParameters: {
    minFeeA: "44",
    minFeeB: "155381",
  },
  submissionLimits: {
    maxSubmitTxCborBytes: 32768,
  },
  validation: {
    strictnessProfile: "phase1_midgard",
    localValidationIsAuthoritative: false,
  },
};

const jsonResponse = (payload: unknown, status = 200): Response =>
  new Response(JSON.stringify(payload), {
    status,
    headers: { "content-type": "application/json" },
  });

const encodedUtxo = (ref: OutRef = outRef) => ({
  outref: outRefToCbor(ref).toString("hex"),
  outputCbor: encodeMidgardTxOutput(address, {
    lovelace: 2_000_000n,
  }).toString("hex"),
});

const makeOtherAddress = (): string =>
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(
      CML.PrivateKey.generate_ed25519().to_public().hash(),
    ),
  )
    .to_address()
    .to_bech32();

const makeProvider = (fetchImpl: MidgardFetch): Promise<MidgardNodeProvider> =>
  MidgardNodeProvider.create({
    endpoint: "http://127.0.0.1:3000/",
    fetch: async (input, init) => {
      const url = new URL(String(input));
      if (url.pathname === "/protocol-info") {
        return jsonResponse(protocolInfoJson);
      }
      return fetchImpl(input, init);
    },
  });

describe("MidgardNodeProvider", () => {
  it("rejects direct runtime construction so protocol-info cannot be bypassed", () => {
    const UnsafeConstructor = MidgardNodeProvider as unknown as new (
      options: object,
    ) => MidgardNodeProvider;

    expect(
      () =>
        new UnsafeConstructor({
          endpoint: "http://127.0.0.1:3000",
          fetch: async () => jsonResponse(protocolInfoJson),
        }),
    ).toThrow(ProviderCapabilityError);
  });

  it("fetches and decodes UTxOs by address", async () => {
    const provider = await makeProvider(async (input) => {
      const url = new URL(String(input));
      expect(url.pathname).toBe("/utxos");
      expect(url.searchParams.get("address")).toBe(address);
      return jsonResponse({ utxos: [encodedUtxo()] });
    });

    await expect(provider.getUtxos(address)).resolves.toMatchObject([
      {
        txHash: outRef.txHash,
        outputIndex: outRef.outputIndex,
        output: {
          address,
          assets: { lovelace: 2_000_000n },
        },
      },
    ]);
  });

  it("rejects malformed UTxO payloads", async () => {
    const provider = await makeProvider(async () =>
      jsonResponse({ utxos: [{ outref: "zz", outputCbor: "00" }] }),
    );

    await expect(provider.getUtxos(address)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
  });

  it("wraps malformed UTxO output bytes as provider payload errors", async () => {
    const provider = await makeProvider(async () =>
      jsonResponse({
        utxos: [
          {
            outref: outRefToCbor(outRef).toString("hex"),
            outputCbor: "00",
          },
        ],
      }),
    );

    await expect(provider.getUtxos(address)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
  });

  it("rejects UTxO address-query responses for other addresses", async () => {
    const otherAddress = makeOtherAddress();
    const otherOutput = encodeMidgardTxOutput(otherAddress, {
      lovelace: 2_000_000n,
    }).toString("hex");
    const provider = await makeProvider(async () =>
      jsonResponse({
        utxos: [
          {
            ...encodedUtxo(),
            outputCbor: otherOutput,
          },
        ],
      }),
    );

    await expect(provider.getUtxos(address)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
  });

  it("fetches UTxOs by outref and returns undefined for misses", async () => {
    const hits = await makeProvider(async (input) => {
      const url = new URL(String(input));
      expect(url.pathname).toBe("/utxo");
      expect(url.searchParams.get("txOutRef")).toBe(
        outRefToCbor(outRef).toString("hex"),
      );
      return jsonResponse({ utxo: encodedUtxo() });
    });
    const miss = await makeProvider(async () =>
      jsonResponse({ error: "missing" }, 404),
    );

    await expect(hits.getUtxoByOutRef(outRef)).resolves.toMatchObject({
      txHash: outRef.txHash,
    });
    await expect(miss.getUtxoByOutRef(outRef)).resolves.toBeUndefined();
  });

  it("rejects substituted UTxOs for outref lookups", async () => {
    const otherOutRef = {
      txHash: "22".repeat(32),
      outputIndex: 0,
    };
    const single = await makeProvider(async () =>
      jsonResponse({ utxo: encodedUtxo(otherOutRef) }),
    );
    await expect(single.getUtxoByOutRef(outRef)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );

    const batch = await makeProvider(async () =>
      jsonResponse({ utxos: [encodedUtxo(otherOutRef)] }),
    );
    await expect(batch.getUtxosByOutRefs([outRef])).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
  });

  it("fetches UTxOs by outref batch", async () => {
    const provider = await makeProvider(async (_input, init) => {
      expect(init?.method).toBe("POST");
      expect(JSON.parse(String(init?.body))).toEqual([`${outRef.txHash}#0`]);
      return jsonResponse({ utxos: [encodedUtxo()] });
    });

    await expect(provider.getUtxosByOutRefs([outRef])).resolves.toHaveLength(1);
  });

  it("parses protocol info and current slot from the node", async () => {
    const provider = await MidgardNodeProvider.create({
      endpoint: "http://127.0.0.1:3000",
      fetch: async () => jsonResponse(protocolInfoJson),
    });

    await expect(provider.getProtocolInfo()).resolves.toEqual(protocolInfo);
    await expect(provider.getCurrentSlot()).resolves.toBe(123456n);
    await expect(provider.getProtocolParameters()).resolves.toMatchObject({
      minFeeA: 44n,
      minFeeB: 155381n,
      networkId: 0n,
      maxSubmitTxCborBytes: 32768,
    });
    expect(provider.diagnostics().protocolInfoSource).toBe("node");
  });

  it("redacts endpoint credentials and query strings in diagnostics", async () => {
    const provider = await MidgardNodeProvider.create({
      endpoint: "https://user:secret@example.test:8443/base?api_key=hidden",
      fetch: async () => jsonResponse(protocolInfoJson),
    });

    expect(provider.diagnostics().endpoint).toBe(
      "https://example.test:8443/base",
    );
  });

  it("fails closed on missing or incompatible supported script languages", async () => {
    const malformedProtocolInfos = [
      { ...protocolInfoJson, supportedScriptLanguages: undefined },
      {
        ...protocolInfoJson,
        supportedScriptLanguages: [{ name: "PlutusV3", tag: 2 }],
      },
      {
        ...protocolInfoJson,
        supportedScriptLanguages: [
          ...MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
          { name: "PlutusV2", tag: 1 },
        ],
      },
      {
        ...protocolInfoJson,
        supportedScriptLanguages: [
          { name: "PlutusV3", tag: 2 },
          { name: "MidgardV1", tag: 999 },
        ],
      },
      {
        ...protocolInfoJson,
        supportedScriptLanguages: [
          { name: "PlutusV1", tag: 0 },
          { name: "MidgardV1", tag: 128 },
        ],
      },
    ];

    for (const payload of malformedProtocolInfos) {
      await expect(
        MidgardNodeProvider.create({
          endpoint: "http://127.0.0.1:3000",
          fetch: async () => jsonResponse(payload),
        }),
      ).rejects.toBeInstanceOf(ProviderPayloadError);
    }
  });

  it("derives network ids from supported protocol-info networks", async () => {
    const mainnetProvider = await MidgardNodeProvider.create({
      endpoint: "http://127.0.0.1:3000",
      fetch: async () =>
        jsonResponse({
          ...protocolInfoJson,
          network: "Mainnet",
        }),
    });
    await expect(
      mainnetProvider.getProtocolParameters(),
    ).resolves.toMatchObject({
      networkId: 1n,
    });

    const unsupportedProvider = await MidgardNodeProvider.create({
      endpoint: "http://127.0.0.1:3000",
      fetch: async () =>
        jsonResponse({
          ...protocolInfoJson,
          network: "Experimental",
        }),
    });
    await expect(
      unsupportedProvider.getProtocolParameters(),
    ).rejects.toBeInstanceOf(ProviderPayloadError);
  });

  it("fails closed when protocol-info is unavailable without fallback", async () => {
    await expect(
      MidgardNodeProvider.create({
        endpoint: "http://127.0.0.1:3000",
        fetch: async () => new Response("not found", { status: 404 }),
      }),
    ).rejects.toBeInstanceOf(ProviderCapabilityError);
  });

  it("uses explicit protocol-info fallback with diagnostics", async () => {
    const provider = await MidgardNodeProvider.create({
      endpoint: "http://127.0.0.1:3000",
      fetch: async () => new Response("not found", { status: 404 }),
      protocolInfoFallback: {
        protocolInfo,
        reason: "test-only explicit fallback",
      },
    });

    await expect(provider.getProtocolInfo()).resolves.toEqual(protocolInfo);
    expect(provider.diagnostics()).toMatchObject({
      protocolInfoSource: "fallback",
      protocolInfoFallbackReason: "test-only explicit fallback",
    });
  });

  it("validates explicit protocol-info fallback at runtime", async () => {
    await expect(
      MidgardNodeProvider.create({
        endpoint: "http://127.0.0.1:3000",
        fetch: async () => new Response("not found", { status: 404 }),
        protocolInfoFallback: {
          protocolInfo: {
            ...protocolInfo,
            currentSlot: "123" as unknown as bigint,
          },
          reason: "invalid fallback",
        },
      }),
    ).rejects.toBeInstanceOf(ProviderPayloadError);

    await expect(
      MidgardNodeProvider.create({
        endpoint: "http://127.0.0.1:3000",
        fetch: async () => new Response("not found", { status: 404 }),
        protocolInfoFallback: {
          protocolInfo: {
            ...protocolInfo,
            protocolFeeParameters: undefined,
          } as unknown as MidgardProtocolInfo,
          reason: "invalid fallback",
        },
      }),
    ).rejects.toBeInstanceOf(ProviderPayloadError);
  });

  it("preserves rejected transaction status code and detail", async () => {
    const provider = await makeProvider(async () =>
      jsonResponse({
        txId: outRef.txHash,
        status: "rejected",
        reasonCode: "E_BAD_TX",
        reasonDetail: "bad",
        timestamps: { createdAt: "2026-05-01T00:00:00.000Z" },
      }),
    );

    await expect(provider.getTxStatus(outRef.txHash)).resolves.toEqual({
      kind: "rejected",
      txId: outRef.txHash,
      code: "E_BAD_TX",
      detail: "bad",
      createdAt: "2026-05-01T00:00:00.000Z",
    });
  });

  it("rejects tx-status responses for a different tx id", async () => {
    const provider = await makeProvider(async () =>
      jsonResponse({
        txId: "22".repeat(32),
        status: "accepted",
      }),
    );

    await expect(provider.getTxStatus(outRef.txHash)).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
  });

  it("classifies unsupported tx-status as a capability error", async () => {
    const provider = await makeProvider(
      async () => new Response("not implemented", { status: 501 }),
    );

    await expect(provider.getTxStatus(outRef.txHash)).rejects.toBeInstanceOf(
      ProviderCapabilityError,
    );
  });

  it("submits new and duplicate transactions with durable admission metadata", async () => {
    const responses = [
      jsonResponse(
        {
          txId: outRef.txHash,
          status: "queued",
          firstSeenAt: "2026-05-01T00:00:00.000Z",
          lastSeenAt: "2026-05-01T00:00:00.000Z",
          duplicate: false,
        },
        202,
      ),
      jsonResponse(
        {
          txId: outRef.txHash,
          status: "queued",
          firstSeenAt: "2026-05-01T00:00:00.000Z",
          lastSeenAt: "2026-05-01T00:00:01.000Z",
          duplicate: true,
        },
        200,
      ),
    ];
    const provider = await makeProvider(async () => responses.shift()!);

    await expect(provider.submitTx("00")).resolves.toMatchObject({
      txId: outRef.txHash,
      httpStatus: 202,
      duplicate: false,
    });
    await expect(provider.submitTx("00")).resolves.toMatchObject({
      txId: outRef.txHash,
      httpStatus: 200,
      duplicate: true,
    });
  });

  it("rejects malformed or oversized direct submit payloads before posting", async () => {
    let submitCalls = 0;
    const provider = await makeProvider(async (input) => {
      const url = new URL(String(input));
      if (url.pathname === "/submit") {
        submitCalls += 1;
      }
      return jsonResponse(
        {
          txId: outRef.txHash,
          status: "queued",
          duplicate: false,
        },
        202,
      );
    });

    await expect(provider.submitTx("zz")).rejects.toBeInstanceOf(
      ProviderPayloadError,
    );
    await expect(
      provider.submitTx(
        "00".repeat(protocolInfo.submissionLimits.maxSubmitTxCborBytes + 1),
      ),
    ).rejects.toBeInstanceOf(ProviderPayloadError);
    expect(submitCalls).toBe(0);
  });

  it("rejects malformed durable submit admission metadata", async () => {
    const malformedResponses = [
      jsonResponse(
        {
          txId: "not-a-tx-id",
          status: "queued",
          duplicate: false,
        },
        202,
      ),
      jsonResponse(
        {
          txId: outRef.txHash,
          status: "mystery",
          duplicate: false,
        },
        202,
      ),
      jsonResponse(
        {
          txId: outRef.txHash,
          status: "queued",
        },
        202,
      ),
      jsonResponse(
        {
          txId: outRef.txHash,
          status: "queued",
          duplicate: true,
        },
        202,
      ),
      jsonResponse(
        {
          txId: outRef.txHash,
          status: "queued",
          duplicate: false,
        },
        200,
      ),
      jsonResponse(
        {
          txId: outRef.txHash,
          status: "accepted",
          duplicate: false,
        },
        202,
      ),
      jsonResponse(
        {
          txId: outRef.txHash,
          status: "queued",
          firstSeenAt: 123,
          duplicate: false,
        },
        202,
      ),
      jsonResponse(
        {
          txId: outRef.txHash,
          status: "queued",
          lastSeenAt: 123,
          duplicate: false,
        },
        202,
      ),
    ];

    for (const response of malformedResponses) {
      const provider = await makeProvider(async () => response);
      await expect(provider.submitTx("00")).rejects.toBeInstanceOf(
        ProviderPayloadError,
      );
    }
  });

  it("classifies conflict, backlog, and transport submit failures", async () => {
    const conflict = await makeProvider(async () =>
      jsonResponse({ error: "E_TX_ID_BYTES_CONFLICT" }, 409),
    );
    await expect(conflict.submitTx("00")).rejects.toMatchObject({
      statusCode: 409,
      retryable: false,
    });

    const backlog = await makeProvider(
      async () => new Response("busy", { status: 503 }),
    );
    await expect(backlog.submitTx("00")).rejects.toMatchObject({
      statusCode: 503,
      retryable: true,
    });

    const transport = await makeProvider(async () => {
      throw new Error("connection reset");
    });
    await expect(transport.submitTx("00")).rejects.toBeInstanceOf(
      ProviderTransportError,
    );
  });

  it("rejects invalid protocol current slot strings", async () => {
    await expect(
      MidgardNodeProvider.create({
        endpoint: "http://127.0.0.1:3000",
        fetch: async () =>
          jsonResponse({ ...protocolInfoJson, currentSlot: "-1" }),
      }),
    ).rejects.toBeInstanceOf(ProviderPayloadError);

    await expect(
      MidgardNodeProvider.create({
        endpoint: "http://127.0.0.1:3000",
        fetch: async () =>
          jsonResponse({ ...protocolInfoJson, currentSlot: 123456 }),
      }),
    ).rejects.toBeInstanceOf(ProviderPayloadError);
  });
});
