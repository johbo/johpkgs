

# Find executables of globally installed npm packages
if [ -d $HOME/.npm-packages/bin ]
then
    export PATH=$HOME/.npm-packages/bin:$PATH
fi

if [ -d $HOME/bin ]
then
    export PATH=$HOME/bin:$PATH
fi
