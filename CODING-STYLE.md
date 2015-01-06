# Coding Style

We use Linux Kernel Coding Style with a few amendments. These are described in
the next section.  

The Linux Kernel Coding Style guidelines can be obtained from here:  
https://www.kernel.org/doc/Documentation/CodingStyle  


## Amendments

### Chapter 8. Comments
#### Paragraph §3

~~When commenting the kernel API functions, please use the kernel-doc format.
See the files Documentation/kernel-doc-nano-HOWTO.txt and scripts/kernel-doc
for details.~~

Instead of kernel-doc, we use Doxygen/Javadoc syntax for documenting code.

When commenting PAL API functions, please use the following format.

```c
/**
 * Short and to the point one line description.
 *
 * Longer text (if needed) describing the function in more detail. It can
 * span multiple lines and paragraphs.
 *
 * @param dest	Destination pointer.
 * @param src	Source pointer.
 * @param n	Number of bytes to copy.
 * @return	A pointer to dest.
 */
void *memcpy(void *dest, const void *src, size_t n)
{
	...
}

```

