import type { FindBlogPosts, FindBlogPostsVariables } from 'types/graphql'

import type {
  CellSuccessProps,
  CellFailureProps,
  TypedDocumentNode,
} from '@redwoodjs/web'

export const QUERY: TypedDocumentNode<FindBlogPosts, FindBlogPostsVariables> = gql`
  query FindBlogPosts {
    blogPosts {
      id
    }
  }
`

export const Loading = () => <div>Loading...</div>

export const Empty = () => <div>Empty</div>

export const Failure = ({ error }: CellFailureProps<FindBlogPostsVariables>) => (
  <div style={{ color: 'red' }}>Error: {error?.message}</div>
)

export const Success = ({
  blogPosts,
}: CellSuccessProps<FindBlogPosts, FindBlogPostsVariables>) => {
  return (
    <ul>
      {blogPosts.map((item) => {
        return <li key={item.id}>{JSON.stringify(item)}</li>
      })}
    </ul>
  )
}
