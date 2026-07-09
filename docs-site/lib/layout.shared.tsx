import Image from 'next/image';
import type { BaseLayoutProps } from 'fumadocs-ui/layouts/shared';
import { MessageCircle, Twitter } from 'lucide-react';

export const gitConfig = {
  user: 'Anastasia-Labs',
  repo: 'midgard',
  branch: 'tx-validation',
};

export function baseOptions(): BaseLayoutProps {
  return {
    nav: {
      title: (
        <>
          <Image
            src="/assets/logo-dark.png"
            alt="Midgard"
            width={122}
            height={28}
            className="hidden shrink-0 dark:block"
          />
          <Image
            src="/assets/logo-light.png"
            alt="Midgard"
            width={122}
            height={28}
            className="block shrink-0 dark:hidden"
          />
        </>
      ),
    },
    links: [
      {
        type: 'icon',
        icon: <MessageCircle />,
        text: 'Discord',
        url: 'https://discord.gg/ZpjgHKWaZx',
        external: true,
      },
      {
        type: 'icon',
        icon: <Twitter />,
        text: 'X',
        url: 'https://x.com/midgardprotocol',
        external: true,
      },
    ],
    githubUrl: `https://github.com/${gitConfig.user}/${gitConfig.repo}`,
  };
}
