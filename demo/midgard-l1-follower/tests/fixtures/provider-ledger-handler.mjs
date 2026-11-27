// The fake sidecar handler of the provider tests: a node ledger serving
// canned local-state-query answers (options.answers, hex by query name), a
// UTxO set (options.utxos: [{txHash, index, address, output}] in hex), a
// mempool (options.mempool: tx ids) and a submit verdict (options.submit:
// "accept", or {rejection: hex}). options.refuse maps a query name to a
// sidecar refusal code.

const head = (major, length) => {
  if (length < 24) return Buffer.from([(major << 5) | length]);
  if (length < 0x100) return Buffer.from([(major << 5) | 24, length]);
  const bytes = Buffer.alloc(3);
  bytes[0] = (major << 5) | 25;
  bytes.writeUInt16BE(length, 1);
  return bytes;
};

/** `{[txHash, index] => output}`, the outputs embedded as their raw bytes. */
const utxoAnswer = (entries) =>
  Buffer.concat([
    head(5, entries.length),
    ...entries.flatMap((entry) => [
      head(4, 2),
      head(2, 32),
      Buffer.from(entry.txHash, "hex"),
      head(0, entry.index),
      Buffer.from(entry.output, "hex"),
    ]),
  ]);

export default (options) => ({
  hello: () => ({ nodeToClientVersion: 32784 }),
  ledgerQuery: (query) => {
    const code = options.refuse?.[query.query];
    if (code !== undefined) return { error: { code, message: "refused" } };
    const utxos = options.utxos ?? [];
    if (query.query === "utxo_by_address") {
      const wanted = new Set(
        query.addresses.map((address) => Buffer.from(address).toString("hex")),
      );
      return utxoAnswer(utxos.filter((utxo) => wanted.has(utxo.address)));
    }
    if (query.query === "utxo_by_txin") {
      const wanted = new Set(
        query.txIns.map(
          ([txId, index]) => `${Buffer.from(txId).toString("hex")}#${index}`,
        ),
      );
      return utxoAnswer(
        utxos.filter((utxo) => wanted.has(`${utxo.txHash}#${utxo.index}`)),
      );
    }
    const answer = options.answers?.[query.query];
    return answer === undefined ? undefined : Buffer.from(answer, "hex");
  },
  submit: () =>
    options.submit === "accept"
      ? { accepted: true }
      : options.submit?.rejection !== undefined
        ? { rejection: Buffer.from(options.submit.rejection, "hex") }
        : { error: { code: "node_unavailable", message: "no submit" } },
  hasTx: (txId) => (options.mempool ?? []).includes(txId),
});
