#!/bin/bash

set -e

TMPDIR=$(mktemp -d)

function finish()
{
  rm -rf $TMPDIR
}

trap finish EXIT

curl -o $TMPDIR/facebook_domains 'https://raw.githubusercontent.com/jmdugan/blocklists/master/corporations/facebook/all'
curl -o $TMPDIR/mozilla_domains 'https://raw.githubusercontent.com/jmdugan/blocklists/master/corporations/mozilla/all.txt'
curl -o $TMPDIR/steffens_domains 'https://raw.githubusercontent.com/stepardo/config/master/hosts'
sed -i -e 's/127\.0\.0\.1/0\.0\.0\.0/ig' $TMPDIR/steffens_domains
curl -o $TMPDIR/netguard_domains 'https://www.netguard.me/hosts'
# pihole sources below
curl -o $TMPDIR/stevens_domains 'https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts'
sed -i -e '/^#/ d' $TMPDIR/stevens_domains
curl -o $TMPDIR/malware_domains 'https://mirror1.malwaredomains.com/files/justdomains'
sed -i -e '/^#/ d' $TMPDIR/malware_domains
sed -i -e 's/^/0.0.0.0 /ig' $TMPDIR/malware_domains
curl -o $TMPDIR/cameleon_domains 'http://sysctl.org/cameleon/hosts'
sed -i -e '/^#/ d' $TMPDIR/cameleon_domains
sed -i -e 's/127.0.0.1/0.0.0.0/ig' $TMPDIR/cameleon_domains
curl -o $TMPDIR/zeustracker_domains 'https://zeustracker.abuse.ch/blocklist.php?download=domainblocklist'
sed -i -e '/^#/ d' $TMPDIR/zeustracker_domains
sed -i -r 's/^/0.0.0.0/ig' $TMPDIR/zeustracker_domains
curl -o $TMPDIR/disconnectme_tracking_domains 'https://zeustracker.abuse.ch/blocklist.php?download=domainblocklist'
sed -i -e '/^#/ d' $TMPDIR/disconnectme_tracking_domains
sed -i -r 's/^/0.0.0.0/ig' $TMPDIR/disconnectme_tracking_domains
curl -o $TMPDIR/disconnectme_ads_domains 'https://s3.amazonaws.com/lists.disconnect.me/simple_ad.txt'
sed -i -e '/^#/ d' $TMPDIR/disconnectme_ads_domains
sed -i -r 's/^/0.0.0.0/ig' $TMPDIR/disconnectme_ads_domains
curl -o $TMPDIR/hosts-file_domains 'https://hosts-file.net/ad_servers.txt'
sed -i -e '/^#/ d' $TMPDIR/hosts-file_domains
sed -i -e 's/127.0.0.1/0.0.0.0/ig' $TMPDIR/hosts-file_domains

cat $TMPDIR/*_domains > $TMPDIR/hosts_a
sort $TMPDIR/hosts_a > $TMPDIR/hosts_b
uniq $TMPDIR/hosts_b > hosts

sed -i -e '/^\s/ d' hosts
sed -i -e '/^#/ d' hosts
sed -i -e '/^:/ d' hosts
sed -i -e '/^f/ d' hosts
sed -i -e '/^1/ d' hosts
sed -i -e '/0.0.0.0 0.0.0.0/ d' hosts

