declare const lucid: any;
declare const first: any;
declare const second: any;
declare const address: string;
declare const build: (lucid: unknown) => Promise<unknown>;

export const leaks = async () => {
  // ruleid: midgard/scoped-utxo-override
  lucid.overrideUTxOs(await lucid.utxosAt(address));
  return build(lucid);
};

export const releasesAnotherInstance = async () => {
  try {
    // ruleid: midgard/scoped-utxo-override
    first.overrideUTxOs([]);
    return await build(first);
  } finally {
    second.clearUTxOOverride();
  }
};

export const releasesBeforePinning = () => {
  lucid.clearUTxOOverride();
  // ruleid: midgard/scoped-utxo-override
  lucid.overrideUTxOs([]);
};

export const scoped = async () => {
  try {
    // ok: midgard/scoped-utxo-override
    lucid.overrideUTxOs(await lucid.utxosAt(address));
    return await build(lucid);
  } finally {
    lucid.clearUTxOOverride();
  }
};
