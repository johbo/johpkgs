NIX_LINK=$HOME/.nix-profile

# Triggers loading etc/profile.d
if [ -d $NIX_LINK/etc/profile.d ]
then
    for file in $NIX_LINK/etc/profile.d/*.sh
    do
        # Avoid to load the nix.sh a second time
        if [[ $file == *"/nix.sh"* ]]
        then
            continue
        fi

        source $file
    done
fi
