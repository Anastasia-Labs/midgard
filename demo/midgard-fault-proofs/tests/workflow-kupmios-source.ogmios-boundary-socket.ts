import { CML } from "@lucid-evolution/lucid";
import JSONBig from "json-bigint";

import {
  computeFraudProofRawL1PointId,
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofRawL1Point,
  type FraudProofRawL1WebSocketLike,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "../src/workflow/index.js";

export const hash = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);

export const DEPLOYMENT = hash(1);

export const RELEASE = hash(2);

export const KUP0_HEAD = hash(3);

export const TARGET = hash(4);

export const ANCESTOR = hash(5);

export const TIP = hash(6);

export const EARLIER = hash(9);

export const chainPoint = (
  slot = "400",
  blockHash = TARGET,
  blockNo = "71",
): FraudProofRawL1Point => ({
  slot,
  blockHash,
  blockNo,
  pointId: computeFraudProofRawL1PointId({ slot, blockHash, blockNo }),
});

export const ordinaryTransaction = (fee: bigint) => {
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    CML.TransactionOutputList.new(),
    fee,
  );
  const value = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
  );
  return {
    id: CML.hash_transaction(body).to_hex(),
    cbor: value.to_canonical_cbor_hex(),
  };
};

export const releaseFinality: VerifiedFraudProofReleaseFinalityPolicy = {
  schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  deploymentIdentityDigest: DEPLOYMENT,
  blueprintHash: RELEASE,
  policyDigest: computeFraudProofReleaseFinalityPolicyDigest({
    confirmationDepth: 30,
    automaticRecoveryMaxDepth: 2160,
    deepRollbackPolicy: "automated_rewind_replay_incident-v1",
  }),
  policy: {
    confirmationDepth: 30,
    automaticRecoveryMaxDepth: 2160,
    deepRollbackPolicy: "automated_rewind_replay_incident-v1",
  },
};

export class OgmiosBoundarySocket implements FraudProofRawL1WebSocketLike {
  readonly listeners = new Map<string, ((event: never) => void)[]>();
  nextCount = 0;
  originIntersection = false;
  intersection = { slot: 380, id: ANCESTOR };
  closeCount = 0;
  readonly frames: string[] = [];

  constructor(
    private readonly transactions: readonly unknown[] = [],
    private readonly childAncestor = ANCESTOR,
    private readonly parentHeight = 70,
    private readonly behavior: Readonly<{
      open?: boolean;
      close?: boolean;
      respond?: boolean;
      responseText?: string;
      sendError?: boolean;
      childHeight?: number;
      tipHeight?: number;
      tipSlot?: number;
      mempoolPresent?: boolean;
      submit?: (cbor: string) => Promise<string>;
    }> = {},
  ) {
    if (behavior.open !== false) queueMicrotask(() => this.emit("open", {}));
  }

  removeEventListener(type: string, listener: (event: never) => void): void {
    this.listeners.set(
      type,
      (this.listeners.get(type) ?? []).filter((value) => value !== listener),
    );
  }

  addEventListener(type: string, listener: (event: never) => void): void {
    const listeners = this.listeners.get(type) ?? [];
    listeners.push(listener);
    this.listeners.set(type, listeners);
  }

  send(data: string): void {
    if (this.behavior.sendError)
      throw new Error("ordinary socket send failure");
    if (this.behavior.respond === false) return;
    const request = JSON.parse(data) as {
      readonly id: number;
      readonly method: string;
      readonly params?: {
        readonly points?: readonly ({ slot: number; id: string } | "origin")[];
        readonly transaction?: { readonly cbor: string };
      };
    };
    if (request.method === "findIntersection") {
      const point = request.params!.points![0]!;
      this.originIntersection = point === "origin";
      if (point !== "origin") this.intersection = point;
    }
    if (
      ["submitTransaction", "acquireMempool", "hasTransaction"].includes(
        request.method,
      )
    ) {
      const operation =
        request.method === "submitTransaction"
          ? this.behavior.submit!(request.params!.transaction!.cbor).then(
              (id) => ({ transaction: { id } }),
            )
          : Promise.resolve(
              request.method === "acquireMempool"
                ? { acquired: "mempool", slot: 1000 }
                : (this.behavior.mempoolPresent ?? false),
            );
      void operation.then((result) =>
        this.emit("message", {
          data: JSON.stringify({ jsonrpc: "2.0", id: request.id, result }),
        }),
      );
      return;
    }
    const result =
      request.method === "findIntersection"
        ? {
            intersection: this.originIntersection
              ? "origin"
              : this.intersection,
            tip: {
              slot: this.behavior.tipSlot ?? 1000,
              id: TIP,
              height: this.behavior.tipHeight ?? 100,
            },
          }
        : this.nextCount++ === 0
          ? { direction: "backward", point: this.intersection }
          : {
              direction: "forward",
              block:
                this.intersection.slot < 380
                  ? {
                      slot: 380,
                      id: ANCESTOR,
                      height: this.parentHeight,
                      ancestor: EARLIER,
                      transactions: this.transactions,
                    }
                  : {
                      slot: 400,
                      id: TARGET,
                      height: this.behavior.childHeight ?? 71,
                      ancestor: this.childAncestor,
                      transactions: this.transactions,
                    },
            };
    const text =
      this.behavior.responseText ??
      JSON.stringify({ jsonrpc: "2.0", id: request.id, result });
    this.frames.push(text);
    queueMicrotask(() =>
      this.emit("message", {
        data: text,
      }),
    );
  }

  close(): void {
    this.closeCount += 1;
    if (this.behavior.close !== false)
      this.emit("close", { code: 1000, reason: "", wasClean: true });
  }

  emit(type: string, event: unknown): void {
    for (const listener of this.listeners.get(type) ?? []) {
      listener(event as never);
    }
  }
}

export const response = (
  value: unknown,
  checkpointHeaders = false,
  oversized = false,
  headHash = KUP0_HEAD,
): Response =>
  new Response(JSONBig.stringify(value), {
    status: 200,
    headers: checkpointHeaders
      ? {
          "content-type": "application/json",
          "x-most-recent-checkpoint": "990",
          etag: headHash,
          ...(oversized ? { "content-length": "67108865" } : {}),
        }
      : { "content-type": "application/json" },
  });
