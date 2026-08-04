/**
 * Type-check fixture for the code sample rendered on the homepage.
 *
 * The homepage sample lives inside a JSX string literal in `page.tsx`, so the
 * twoslash transformer that guards the MDX snippets cannot reach it. This file
 * contains the same calls against the real `@al-ft/lucid-midgard` types, so
 * `next build` fails if the sample names an export or method that no longer
 * exists.
 *
 * Keep this file and the `transfer.ts` block in `page.tsx` in step.
 */
import { LucidMidgard, MidgardNodeProvider } from '@al-ft/lucid-midgard';

export async function homepageSample(recipient: string, seedPhrase: string) {
  const provider = await MidgardNodeProvider.create({
    endpoint: 'http://127.0.0.1:3000',
  });

  const midgard = await LucidMidgard.new(provider, 'Preprod');
  midgard.selectWallet.fromSeed(seedPhrase);

  const tx = await midgard
    .newTx()
    .pay.ToAddress(recipient, { lovelace: 5_000_000n })
    .complete();

  const signed = await tx.sign.withWallet().complete();
  const submitted = await signed.submit();
  await submitted.awaitStatus({ until: 'accepted' });
}
