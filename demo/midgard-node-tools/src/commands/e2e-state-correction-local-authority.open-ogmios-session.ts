import {
  normalizeOgmiosWebSocketUrl,
  type WebSocketFactory,
  type WebSocketLike,
} from "midgard-node/l1-tx-order-carriage";

import {
  HEX_28,
  HEX_32,
  type LiveEconomicTransaction,
  type LiveKupoOutput,
  type LiveTransactionOutput,
  lowerHex,
  nonNegativeInteger,
  record,
  unsignedDecimal,
} from "./e2e-state-correction-local-authority.fetch-json.js";

export const parseKupoOutputs = (
  value: unknown,
  field: string,
): readonly LiveKupoOutput[] => {
  if (!Array.isArray(value)) throw new Error(`${field} must be an array`);
  return value.map((entry, index) => {
    const itemField = `${field}[${index.toString()}]`;
    const item = record(entry, itemField);
    const valueRecord = record(item.value, `${itemField}.value`);
    const assets = record(valueRecord.assets, `${itemField}.value.assets`);
    const normalizedAssets: Record<string, string> = {};
    for (const [unit, quantity] of Object.entries(assets)) {
      const normalizedUnit = unit.replaceAll(".", "");
      if (!/^[0-9a-f]{56,120}$/u.test(normalizedUnit)) {
        throw new Error(
          `${itemField}.value.assets.${unit} is not an asset unit`,
        );
      }
      normalizedAssets[normalizedUnit] = unsignedDecimal(
        quantity,
        `${itemField}.value.assets.${unit}`,
      );
    }
    return {
      txHash: lowerHex(
        item.transaction_id,
        HEX_32,
        `${itemField}.transaction_id`,
      ),
      outputIndex: nonNegativeInteger(
        item.output_index,
        `${itemField}.output_index`,
      ),
      address:
        typeof item.address === "string" && item.address.length > 0
          ? item.address
          : (() => {
              throw new Error(`${itemField}.address is missing`);
            })(),
      lovelace: unsignedDecimal(valueRecord.coins, `${itemField}.value.coins`),
      spent: item.spent_at !== null,
      assets: normalizedAssets,
    };
  });
};

const parseOgmiosValue = (
  value: unknown,
  field: string,
): LiveTransactionOutput["assets"] & { readonly lovelace: string } => {
  const valueRecord = record(value, field);
  const ada = record(valueRecord.ada, `${field}.ada`);
  const assets: Record<string, string> = {};
  for (const [policyId, rawPolicyAssets] of Object.entries(valueRecord)) {
    if (policyId === "ada") continue;
    if (!HEX_28.test(policyId)) {
      throw new Error(`${field}.${policyId} is not a policy id`);
    }
    const policyAssets = record(rawPolicyAssets, `${field}.${policyId}`);
    for (const [assetName, quantity] of Object.entries(policyAssets)) {
      if (!/^(?:[0-9a-f]{2}){0,32}$/u.test(assetName)) {
        throw new Error(
          `${field}.${policyId}.${assetName} is not an asset name`,
        );
      }
      assets[`${policyId}${assetName}`] = unsignedDecimal(
        quantity,
        `${field}.${policyId}.${assetName}`,
      );
    }
  }
  return {
    lovelace: unsignedDecimal(ada.lovelace, `${field}.ada.lovelace`),
    ...assets,
  };
};

export const parseOgmiosEconomicTransaction = (
  value: unknown,
  txHash: string,
  field: string,
): LiveEconomicTransaction => {
  const transaction = record(value, field);
  if (transaction.id !== txHash) {
    throw new Error(`${field}.id does not match ${txHash}`);
  }
  const fee = parseOgmiosValue(transaction.fee, `${field}.fee`);
  if (Object.keys(fee).some((key) => key !== "lovelace")) {
    throw new Error(`${field}.fee contains a non-ADA asset`);
  }
  if (!Array.isArray(transaction.outputs)) {
    throw new Error(`${field}.outputs must be an array`);
  }
  const outputs = transaction.outputs.map((value, index) => {
    const outputField = `${field}.outputs[${index.toString()}]`;
    const output = record(value, outputField);
    if (typeof output.address !== "string" || output.address.length === 0) {
      throw new Error(`${outputField}.address is missing`);
    }
    const parsedValue = parseOgmiosValue(output.value, `${outputField}.value`);
    const { lovelace, ...assets } = parsedValue;
    return { address: output.address, lovelace, assets };
  });
  const references =
    transaction.references === undefined
      ? []
      : Array.isArray(transaction.references)
        ? transaction.references.map((value, index) => {
            const reference = record(
              value,
              `${field}.references[${index.toString()}]`,
            );
            const referencedTransaction = record(
              reference.transaction,
              `${field}.references[${index.toString()}].transaction`,
            );
            return `${lowerHex(
              referencedTransaction.id,
              HEX_32,
              `${field}.references[${index.toString()}].transaction.id`,
            )}#${nonNegativeInteger(
              reference.index,
              `${field}.references[${index.toString()}].index`,
            ).toString()}`;
          })
        : (() => {
            throw new Error(`${field}.references must be an array`);
          })();
  const inputs = Array.isArray(transaction.inputs)
    ? transaction.inputs.map((value, index) => {
        const input = record(value, `${field}.inputs[${index.toString()}]`);
        const inputTransaction = record(
          input.transaction,
          `${field}.inputs[${index.toString()}].transaction`,
        );
        return `${lowerHex(
          inputTransaction.id,
          HEX_32,
          `${field}.inputs[${index.toString()}].transaction.id`,
        )}#${nonNegativeInteger(
          input.index,
          `${field}.inputs[${index.toString()}].index`,
        ).toString()}`;
      })
    : (() => {
        throw new Error(`${field}.inputs must be an array`);
      })();
  return {
    feeLovelace: fee.lovelace,
    inputs,
    referenceInputs: references,
    outputs,
  };
};

type OgmiosSession = {
  readonly request: (
    method: string,
    params: Readonly<Record<string, unknown>>,
  ) => Promise<unknown>;
  readonly close: () => void;
};

export const openOgmiosSession = async ({
  ogmiosUrl,
  timeoutMs,
  webSocketFactory,
}: {
  readonly ogmiosUrl: string;
  readonly timeoutMs: number;
  readonly webSocketFactory?: WebSocketFactory;
}): Promise<OgmiosSession> => {
  const socket: WebSocketLike = (
    webSocketFactory ??
    ((url: string) => new WebSocket(url) as unknown as WebSocketLike)
  )(normalizeOgmiosWebSocketUrl(ogmiosUrl));
  const pending = new Map<
    number,
    {
      readonly resolve: (value: unknown) => void;
      readonly reject: (error: Error) => void;
    }
  >();
  let nextId = 0;
  let terminal: Error | null = null;
  const fail = (error: Error): void => {
    terminal ??= error;
    for (const waiter of pending.values()) waiter.reject(error);
    pending.clear();
  };
  socket.addEventListener("message", ((event: { readonly data: unknown }) => {
    if (typeof event.data !== "string") {
      fail(new Error("Q57 Ogmios chain-sync sent a non-text frame"));
      return;
    }
    let message: {
      readonly id?: unknown;
      readonly result?: unknown;
      readonly error?: unknown;
    };
    try {
      message = JSON.parse(event.data) as typeof message;
    } catch (cause) {
      fail(new Error("Q57 Ogmios chain-sync sent malformed JSON", { cause }));
      return;
    }
    if (typeof message.id !== "number") return;
    const waiter = pending.get(message.id);
    if (waiter === undefined) return;
    pending.delete(message.id);
    if (message.error !== undefined) {
      waiter.reject(
        new Error(
          `Q57 Ogmios chain-sync error: ${JSON.stringify(message.error)}`,
        ),
      );
    } else {
      waiter.resolve(message.result);
    }
  }) as (event: never) => void);
  socket.addEventListener("error", (() => {
    fail(new Error("Q57 Ogmios chain-sync socket failed"));
  }) as (event: never) => void);
  socket.addEventListener("close", (() => {
    fail(new Error("Q57 Ogmios chain-sync socket closed"));
  }) as (event: never) => void);
  await new Promise<void>((resolve, reject) => {
    const timeout = setTimeout(() => {
      socket.close();
      reject(new Error("Q57 Ogmios chain-sync open timed out"));
    }, timeoutMs);
    socket.addEventListener(
      "open",
      (() => {
        clearTimeout(timeout);
        resolve();
      }) as (event: never) => void,
      { once: true },
    );
    socket.addEventListener(
      "error",
      (() => {
        clearTimeout(timeout);
        reject(new Error("Q57 Ogmios chain-sync failed while opening"));
      }) as (event: never) => void,
      { once: true },
    );
  });
  return {
    request: async (method, params) => {
      if (terminal !== null) throw terminal;
      const id = nextId++;
      return await new Promise<unknown>((resolve, reject) => {
        const timeout = setTimeout(() => {
          pending.delete(id);
          reject(new Error(`Q57 Ogmios ${method} timed out`));
        }, timeoutMs);
        pending.set(id, {
          resolve: (result) => {
            clearTimeout(timeout);
            resolve(result);
          },
          reject: (error) => {
            clearTimeout(timeout);
            reject(error);
          },
        });
        socket.send(JSON.stringify({ jsonrpc: "2.0", method, params, id }));
      });
    },
    close: () => socket.close(),
  };
};
