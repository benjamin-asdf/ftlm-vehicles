# I need to do this to publish for the alpine instance
export PATH="/usr/lib/jvm/java-17-openjdk/bin:$PATH"
export JAVA_HOME="/usr/lib/jvm/java-17-openjdk/"

bb build
