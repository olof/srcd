plan 30

description <<'EOF'
Initiailize a default set of repos and perform write operations
on them.
EOF

fixture mutations

start

for PROTO_VERSION in 0 1 2; do
	v=$PROTO_VERSION

	output=$(git clone "$BASE_URL/test$v.git" 2>&1)
	r=$?
	is $r 0 "cloning test$v.git succeeds"
	[ $r -eq 0 ] || echo "$output" >&2

	is "$(git -C test$v rev-parse HEAD)" \
	   "399a62bb6d769a769f5ebc6007c8e0db80a251c4" \
	   "test$v.git starts at expected commit"

	# Test that we can push commits with no new tree/blob objects
	git -C test$v commit -q --allow-empty --message 'nochange'

	is "$(git -C test$v rev-parse HEAD)" \
	   "1efefe3f4b4018e4f053a6d47a238537f5b1bc81" \
	   "test$v.git locally updated to 1efefe3"

	git -C test$v push origin HEAD:refs/heads/master
	is $r 0 "pushing test$v.git succeeds"

	output=$(git clone "$BASE_URL/test$v.git" test$v-copy1 2>&1)
	r=$?
	is $r 0 "cloning test$v.git again succeeds"
	[ $r -eq 0 ] || echo "$output" >&2

	is "$(git -C test$v-copy1 rev-parse HEAD)" \
	   "1efefe3f4b4018e4f053a6d47a238537f5b1bc81" \
	   "new clone of test$v.git has updated rev"

	# Test that we can push real changes
	echo >>test$v/file
	git -C test$v add file
	git -C test$v commit -q --message 'test change'

        #"73bcc819355b1b4e7bd3466e3f8233726515656d"
	is "$(git -C test$v rev-parse HEAD)" \
	   "25d4d5b4807f4c6bd68b21d031d1e30a8088b01b" \
	   "test$v.git locally updated to 73bcc81"

	git -C test$v push origin HEAD:refs/heads/master
	r=$?
	is $r 0 "pushing test$v.git succeeds"
	[ $r -eq 0 ] || echo "$output" >&2

	output=$(git clone "$BASE_URL/test$v.git" test$v-copy2 2>&1)
	r=$?
	is $r 0 "cloning test$v.git again succeeds"
	[ $r -eq 0 ] || echo "$output" >&2

        # "73bcc819355b1b4e7bd3466e3f8233726515656d"
	is "$(git -C test$v-copy2 rev-parse HEAD)" \
	   "25d4d5b4807f4c6bd68b21d031d1e30a8088b01b" \
	   "new clone of test$v.git has updated rev"
done
PROTO_VERSION=

:
