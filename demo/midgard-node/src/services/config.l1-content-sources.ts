import { Config } from "effect";

/**
 * Where the node may fetch an L1 transaction by id when the local node's
 * ledger no longer holds a forced order's carriage output (plan §12.3 step
 * 4). `L1_TX_CONTENT_SOURCES` is a comma-separated list of URL templates,
 * each with `{txId}` where the transaction id goes, answering with the
 * transaction's CBOR (raw, hex text, or JSON `{ "cbor": hex }`). Every answer is accepted only if its body hashes to the
 * id asked for, so a source can delay an order's ingestion, never change it.
 * Unset means none: only the follower's own blocks and the local node serve.
 */
export const l1ContentSourcesConfig = Config.all({
  L1_TX_CONTENT_SOURCES: Config.string("L1_TX_CONTENT_SOURCES").pipe(
    Config.withDefault(""),
    Config.mapAttempt((value): readonly string[] => {
      const templates = value
        .split(",")
        .map((template) => template.trim())
        .filter((template) => template !== "");
      for (const template of templates) {
        if (!template.includes("{txId}"))
          throw new Error(
            `L1_TX_CONTENT_SOURCES entry ${template} has no {txId} placeholder`,
          );
        new URL(template.replace("{txId}", "0"));
      }
      return templates;
    }),
  ),
});
