/**
 * The MPF error channel.
 */

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "effect";

export class MpfError extends Data.TaggedError(
  "MpfError",
)<SDK.GenericErrorFields> {
  static get(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred getting an entry from ${trie} MPF`,
      cause,
    });
  }

  static insert(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred inserting a new entry in ${trie} MPF`,
      cause,
    });
  }

  static delete(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred deleting an entry from ${trie} MPF`,
      cause,
    });
  }

  static batch(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred during a batch operation on ${trie} MPF`,
      cause,
    });
  }

  static phasRoot(cause: unknown) {
    return new MpfError({
      message: "An error occurred building a Midgard PHAS root or proof",
      cause,
    });
  }

  static rootBuild(rootName: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred building ${rootName} MPF root`,
      cause,
    });
  }

  static create(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred creating ${trie} MPF`,
      cause,
    });
  }

  static close(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred closing ${trie} MPF store`,
      cause,
    });
  }

  static prove(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred proving a key in ${trie} MPF`,
      cause,
    });
  }

  static verify(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred verifying a proof for ${trie} MPF`,
      cause,
    });
  }

  static rootNotSet(trie: string, cause: unknown) {
    return new MpfError({
      message: `An error occurred getting ${trie} MPF root, the root is ${typeof cause}`,
      cause,
    });
  }
}
