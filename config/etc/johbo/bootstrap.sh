NIX_LINK=$HOME/.nix-profile

# TODO: Avoid that the nix-daemon file screws things up
__ETC_PROFILE_NIX_SOURCED=1

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


# Load bash completion
if [ -d $NIX_LINK/etc/bash_completion.d ]
then
    for file in $NIX_LINK/etc/bash_completion.d/*
    do
        source $file
    done
fi
